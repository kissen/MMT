package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import archives._
import symbols._
import modules._
import objects._
import documents._

import info.kwarc.mmt.lf._

class ElpiTableauExporter extends Exporter {
  val key = "lf-elpit"
  override def outExt = "elpit"

  private lazy val lup = controller.globalLookup
  private lazy val ruleMatcher = new RuleMatcher(lup, List("Judgment","Judgement"))

  private def translateTheory(thy: Theory): List[String] = {
    val cons = thy.getDeclarations
    val consE = cons flatMap translateDeclaration
    consE
  }

  private def translateDeclaration(d: Declaration): List[String] = {
    def fail(msg: String) = {
      println(msg)
      List("% skipping due to error: " + d.path + "\n% " + msg)
    }
    d match {
      case c: Constant =>
        val comment = s"% generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}"
        println(s"generated from ${c.name} : ${controller.presenter.asString(c.tp.get)}")
        c.tp match {
          case Some(t) =>
            t match {
              case ruleMatcher.Rule(dr) =>
                try {
                  val rules = translateRule(c, dr)
                  List(comment) ++ rules
                } catch {case ELPIError(msg) =>
                  fail(msg)
                }
              case _ =>
                val msg = "not a rule: " + controller.presenter.asString(t)
                fail(msg)
            }
          case None =>
            throw ELPIError("cannot translate untyped constant: " + c.path)
        }
      case PlainInclude(from,_) =>
        // we generate one elpi file per MMT theory; MMT includes become lambda-Prolog file includes
        getOutFileForModule(from) match {
          case Some(f) => List(s"""accumulate "$f".""")
          case None => throw ELPIError("no ELPI file known for theory " + from)
        }
      case _: RuleConstant =>
        Nil // ignored
      case _: Structure =>
        Nil // redundant after flattening
      case _ =>
        throw ELPIError("unknown declaration: " + d.path)
    }
  }


  // case class ArbitraryNesting[+T](v : Either[List[ArbitraryNesting[T]],T])
//  abstract case class MarkedTerm(T : Term)
//  case class MarkF(t : Term) extends MarkedTerm(t)
//  case class MarkT(t : Term) extends MarkedTerm(t)

  private def translateRule(c: Constant, dr: DeclarativeRule) : List[String] = {
    println("C: " + c)
    val args = dr.arguments.collect {
      case RuleParameter(_,_) => None
      case RuleAssumption(cj) => Some(processRule(cj))
    }.flatten
    val total : List[List[Term]] = args ::: List(dr.conclusion.arguments)
    println("TOTAL")
    println(total)
    println(branchesF(total))
    println(branchesT(total))
    var lines : List[String] = List()
    for (branch <- branchesF(total)) {
      val s = branch.map( f => "markedF (" + controller.presenter.asString(f) + ")").mkString(", ")
      lines = lines ::: List("tab Helper [markedF (" + controller.presenter.asString(total.head.head) + ") | Rest] Processed :- tab Helper [" + s + " | Rest] Processed.")
    }
    for (branch <- branchesT(total)) {
      val s = branch.map( f => "markedT (" + controller.presenter.asString(f) + ")").mkString(", ")
      lines = lines ::: List("tab Helper [markedT (" + controller.presenter.asString(total.head.head) + ") | Rest] Processed :- tab Helper [" + s + " | Rest] Processed.")
    }
    lines
  }

  private def branchesF(l : List[List[Term]]) : List[List[Term]] = {
    l.drop(1)
     .dropRight(1)
     .map(l2 => l2.dropRight(1))
     .foldLeft[List[List[Term]]](List(List()))(
       (x : List[List[Term]],z:List[Term]) => x.flatMap(branch => z.map(t => t::branch))
     )
  }

  private def branchesT(l : List[List[Term]]) : List[List[Term]] = {
    l.drop(1)
     .dropRight(1)
     .map(l2 => l2.dropRight(1))
  }

  private def processRule(cj : ComplexJudgement) : List[Term] = {
    cj match {
      case ComplexJudgement(_, hypotheses, thesis) => {
        (hypotheses:::List(thesis)).map {
          case AtomicJudgement(rl, op, args) => {
            assert(args.length == 1)
            args.head
          }
        }
      }
    }
  }

  def exportTheory(thy: Theory, bf: BuildTask) {
    val thyE = translateTheory(thy)
    rh << thyE.mkString("\n")
  }

  def exportView(view: View, bf: BuildTask) {}
  def exportDocument(doc: Document, bf: BuildTask) {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}
}
