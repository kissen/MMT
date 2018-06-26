package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import symbols._
import documents._
import modules._
import symbols._
import patterns._
import objects._
import frontend._
import opaque._
import notations._
import ontology._

/**
 * The TypedRelationalExtractor adds a few more typing related informations to the .rel files.
 */
class TypedRelationalExtractor extends RelationalExtractor {
   val allUnary = List(IsDocument,ontology.IsTheory,IsView,IsConstant,IsType,IsJudgement,IsKind,IsHighUniverse,
   IsStructure,IsConAss, IsStrAss,IsNotation,IsDerivedDeclaration,IsPattern,IsInstance)
   val allBinary = List(RefersTo,DependsOn,Includes,IsAliasFor,IsInstanceOf,HasMeta,HasDomain,HasCodomain,Declares,
         IsAlignedWith, HasViewFrom)

   def isJudgment(tp: Term): Boolean = tp match {
      case FunType(_, ApplySpine(OMS(s),_)) =>
         //this can throw errors if the implicit graph is not fully loaded
         try {
            controller.globalLookup.getConstant(s).rl.contains("Judgment")
         } catch {case e: Error =>
            false
         }
      case _ => false
   }
           
   /** apply a continuation function to every relational element of a StructuralElement */
   def apply(e: StructuralElement)(implicit f: RelationalElement => Unit) {
      val path = e.path
      e match {
         case d: Document =>
            f(IsDocument(d.path))
            //TODO should use getLocalItems but then it wouldn't work for documents created from folders
            d.getDeclarations.foreach {i =>
               val c = i match {
                  case inner: Document =>
                     apply(inner)
                     inner.path
                  case oe: OpaqueElement => oe.path
                  case nr: NRef => nr.target
               }
               f(Declares(d.path, c))
            }
         case n: NRef =>
         case oe: OpaqueElement =>
         case t: Theory =>
            f(ontology.IsTheory(path))
            t match {
               case t: DeclaredTheory =>
                  t.meta foreach {p => f(HasMeta(path, p))}
               case _ => QueryFunctionApply
            }
         case v: View =>
            f(HasDomain(path, v.from.toMPath))
            f(HasCodomain(path, v.to.toMPath))
            f(IsView(path))
            f(HasViewFrom(v.to.toMPath, v.from.toMPath))
         case _ =>
      }
      e match {
         case t: DeclaredModule =>
            t.getDeclarations foreach {d => {
               val dec = Declares(path,d.path)
               d match {
                  case c: Constant =>
                     f(dec)
                     f(IsConstant(c.path))
                     c.alias foreach {a =>
                       f(IsAliasFor(t.path ? a, c.path))
                     }
                     if (isJudgment(c.toTerm)) {
                       println("Found Judgement: "+c.path); f(IsJudgement(c.path))}
                     else c.tp match {
                         case Some(Univ(1)) => println("Found Type: "+c.path); f(IsType(c.path))
                         case Some(Univ(2)) => println("Found Kind: "+c.path); f(IsKind(c.path))
                         case Some(Univ(n)) if n > 2 => f(IsHighUniverse(c.path))
                         case _ =>
                       }
                  case s: Structure =>
                     val from = s.from match {
                        case OMPMOD(p,_) => p
                        case f => TheoryExp.simplify(s.from).toMPath
                     }
                     if (s.isInclude) {
                        f(Includes(t.path, from))
                     } else {
                        f(dec)
                        f(HasDomain(s.path, from))
                        f(HasCodomain(s.path, TheoryExp.simplify(s.to).toMPath))
                        f(IsStructure(s.path))
                     }
                  case rc: RuleConstant =>
                     f(dec)
                     f(IsConstant(rc.path))
                  case dd: DerivedDeclaration =>
                     f(dec)
                     f(IsDerivedDeclaration(dd.path))
                     dd.feature match {
                       case Pattern.feature => f(IsPattern(dd.path))
                       case Instance.feature =>
                          f(IsInstance(dd.path))
                          dd.tpC.get match {
                            case Some(Instance.Type(p,_)) => f(IsInstanceOf(dd.path, p))
                            case _ =>
                          }
                       case _ =>
                     }
                  case nm: NestedModule =>
                     f(dec)
                     apply(nm.module)
               }
            }}
         case _ =>
      }
      e match {
        case l: Link if l.isImplicit =>
          f(IsImplicitly(l.to.toMPath,l.from.toMPath))
        case _ =>
      }
   }
}



/*class TExtensionManager(controller: Controller) extends ExtensionManager(controller: Controller) {
    val mmtextr = TypedRelationalExtractor
}

class TController extends Controller {
  override val extman = new TExtensionManager(this)
}

trait TExtension extends Extension {
  /** the controller that this extension is added to; only valid after creation of the extension, i.e., will return null if used in a non-lazy val-field */
    /** MMT initialization (idempotent) */
  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
    
  }
}

*/