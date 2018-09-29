package info.kwarc.mmt.lf.structuralfeatures

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._
import frontend.Controller
import info.kwarc.mmt.lf._
import InternalDeclaration._
import InternalDeclarationUtil._

class Quotients extends StructuralFeature("quotient") with ParametricTheoryLike {
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val name = LocalName(dd.path.last)
    implicit val parentTerm = dd.path
    val params = Type.getParameters(dd)
    val context = if (params.nonEmpty) Some(params) else None
    try {
      val eqRel = dd.getDeclarations.last match {case c:Constant=>fromConstant(c, controller, context)}
      val dom = eqRel.args match {
        case List((_, d), (_,_)) => d
        case tp => throw ImplementationError("Unexpectedly found: . ")
      }
      val rel = eqRel.toConstant
      val structure = structureDeclaration(Some("Q"), context)
      
      val quotientMap = {
        val Ltp = () => {
          val tp = Arrow(dom, structure.tp.get)
          PiOrEmpty(params, if (!params.isEmpty) ApplyGeneral(tp, params.map(_.toTerm)) else tp)
        }
        makeConst(uniqueLN("quotient_map"), Ltp)
      }
      val quotMapSurj = surjDecl(quotientMap, controller, context)
      
      val quotInvert = Pushout(dom, structure.path, rel.path, quotientMap.path, Some("quotInvert"), context)
      val quotInverse = inverse(dom, structure.path, rel.path, quotientMap.path, quotInvert.path, Some("quotInverse"), context)
      
      val elabDecls = List(rel, structure, quotientMap, quotInvert, quotInverse, quotMapSurj)
      //elabDecls map {d => log(defaultPresenter(d)(controller))}
      new Elaboration {
        def domain = elabDecls map {d => d.name}
        def getO(n: LocalName) = {
          elabDecls.find(_.name == n)
        }
      }
    } catch {
      case e: Error => throw LocalError("Wrong argument: Quotient expects a relation as an argument: "+e.getMessage)
    }
  }
  def quotientEq(structure: TypeLevel, rel: InternalDeclaration, quot: TermLevel, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): TermLevel = {
    val a = rel.args match {
      case List((_, d), (_,_)) => d
      case tp => println(tp); throw ImplementationError("Unexpected match case. ")
    }
    val args = List(newVar(uniqueLN("a1"), a, None), newVar(uniqueLN("a2"), a, None))
    val ret : Term = PiOrEmpty(args, Arrow(rel.applyTo(args), Eq(quot.applyTo(args.init), quot.applyTo(args.tail))))
    TermLevel(uniqueGN(name getOrElse "quot"), args map (x => (Some(x.name), x.tp.get)), ret, None, None, ctx)
  }
  
  /** Declares the unique push-out of a function on the domain down to the quotient, whenever well-defined */
  def Pushout(dom: Term, Quot:GlobalName, relat:GlobalName, quot: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val (q, rel) = if (!ctx.isEmpty) (ApplyGeneral(OMS(Quot), ctx.get.map(_.toTerm)), ApplyGeneral(OMS(relat), ctx.get.map(_.toTerm))) else (OMS(Quot), OMS(relat))
      val f = newVar(uniqueLN("f"), Arrow(dom, dom), ctx)
      val (x, y) = (newVar(uniqueLN("x"), dom, ctx), newVar(uniqueLN("y"), dom, ctx))
      val proof = Pi(List(x, y), ApplyGeneral(rel, List(x.toTerm, y.toTerm)))
      
      Pi(ctx getOrElse Context.empty ++ f, Arrow(proof, Arrow(q, q)))
    }
    makeConst(uniqueLN(name getOrElse "g"), Ltp)
  }
  
  def inverse(dom: Term, Quot:GlobalName, relat:GlobalName, quot: GlobalName, quotInv: GlobalName, name: Option[String], ctx: Option[Context])(implicit parent: GlobalName): Constant = {
    val Ltp = () => {
      val (q, rel) = if (!ctx.isEmpty) (ApplyGeneral(OMS(Quot), ctx.get.map(_.toTerm)), ApplyGeneral(OMS(relat), ctx.get.map(_.toTerm))) else (OMS(Quot), OMS(relat))
      val f = newVar(uniqueLN("f"), Arrow(dom, dom), ctx)
      val (x, y, z) = (newVar(uniqueLN("x"), dom, ctx), newVar(uniqueLN("y"), dom, ctx), newVar(uniqueLN("z"), dom, ctx))
      val proof = Pi(List(x, y), ApplyGeneral(rel, List(x.toTerm, y.toTerm)))
      
      val g_z = ApplyGeneral(OMS(quotInv), (ctx getOrElse Context.empty).map(_.toTerm) ++ List(f.toTerm, proof, z.toTerm))
      val f_o_quot_z = ApplySpine(OMS(quot), ApplySpine(f.toTerm, z.toTerm))
      Pi((ctx getOrElse Context.empty) ++ f ++ z, Eq(g_z, f_o_quot_z))
    }
  makeConst(uniqueLN(name getOrElse "quotInverse"), Ltp)
  }
}

object QuotientRule extends StructuralFeatureRule(classOf[Quotients], "Quotients")