package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

import libraries._
import modules._
import symbols._
import frontend._
import objects.Conversions._
import scala.collection.mutable.{HashSet,HashMap}

/* ideas
 * inferType guarantees well-formedness (not done yet by LambdaTerm)
 *   but what if there are unknowns whose type cannot be inferred? Is that even possible?
 * limitedSimplify must include computation, definition expansion, but can stop on GlobalChange; but safety is usually needed
 * constants have equality rule: injectivity and implicit arguments used to obtain necessary/sufficient condition (not preserved by morphism); congruence if no rule applicable
 */

object InferredType extends TermProperty[Term](utils.mmt.baseURI / "clientProperties" / "solver" / "inferred")

/**
 * A Solver is used to solve a system of constraints about Term's given as judgments.
 * 
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * variables may represent any MMT term, i.e., object language terms, types, etc.;
 * the solution is a [[Substitution]] that provides a closed Term for every unknown variable.
 * (Higher-order abstract syntax should be used to represent unknown terms with free variables as closed function terms.) 
 * 
 * The Solver decomposes the judgments individually by applying typing rules, collecting (partial) solutions along the way and possibly delaying unsolvable judgments.
 * If the unknown variables are untyped and rules require a certain type, the Solver adds the type.
 * 
 * Unsolvable constraints are delayed and reactivated if later solving of unknowns provides further information.
 * 
 * @param controller an MMT controller that is used to look up Rule's and Constant's. No changes are made to the controller.
 * @param theory the current theory
 * @param initUnknowns the list of all unknown variables including their types and listed in dependency order;
 *   unknown variables may occur in the types of later unknowns.
 * 
 * Use: Create a new instance for every problem, call apply on all constraints, then call getSolution.  
 */
class Solver(val controller: Controller, theory: Term, initUnknowns: Context) extends Logger {
   /** tracks the solution, initially equal to unknowns, then a definiens is added for every solved variable */ 
   private var solution : Context = initUnknowns
   /** the unknowns that were solved since the last call of activate (used to determine which constraints are activatable) */
   private var newsolutions : List[LocalName] = Nil
   /** tracks the delayed constraints, in any order */ 
   private var delayed : List[DelayedConstraint] = Nil
   /** tracks the errors in reverse order of encountering */
   private var errors: List[History] = Nil
   /** tracks the dependencies in reverse order of encountering */
   private var dependencies : List[CPath] = Nil
   /** true if unresolved constraints are left */
   def hasUnresolvedConstraints : Boolean = ! delayed.isEmpty
   /** true if unsolved variables are left */
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
   /** true if all judgments solved so far succeeded (all variables solved, no delayed constraints, no errors) */
   def checkSucceeded = ! hasUnresolvedConstraints && ! hasUnsolvedVariables && errors.isEmpty
   /** the solution to the constraint problem
    * @return None if there are unresolved constraints or unsolved variables; Some(solution) otherwise 
    */
   def getSolution : Option[Substitution] = if (delayed.isEmpty) solution.toSubstitution else None
   /**
    * @return the current partial solution to the constraint problem
    * This solution may contain unsolved variables, and there may be unresolved constraints. 
    */
   def getPartialSolution : Context = solution
   /**
    * @return the context containing only the unsolved variables
    */   
   def getUnsolvedVariables : Context = solution.filter(_.df.isEmpty)
   /**
    * @return the current list of unresolved constraints
    */
   def getConstraints : List[DelayedConstraint] = delayed
   /**
    * @return the current list of errors and their history
    */
   def getErrors : List[History] = errors
   /**
    * @return the current list of dependencies
    */
   def getDependencies : List[CPath] = dependencies

   /** for Logger */ 
   val report = controller.report
   /** prefix used when logging */ 
   var logPrefix = "object-checker"
   /** shortcut for the RuleStore of the controller; used to retrieve Rule's */
   private val ruleStore = controller.extman.ruleStore
   /** the SubstitutionApplier to be used throughout */
   private implicit val sa = new MemoizedSubstitutionApplier
   /** a DefinitionExpander to be used throughout */
   private val defExp = new uom.DefinitionExpander(controller)
   /** shortcut for UOM simplification */
   private def simplify(t : Term) = controller.uom.simplify(t, theory, solution)
   /** used for rendering objects, should be used by rules if they want to log */
   implicit val presentObj : Obj => String = o => controller.presenter.asString(o)
   
   /**
    * logs a string representation of the current state
    * @param prefix the log prefix to use (the normal one by default)
    * (occasionally it's useful to use a different prefix, e.g., "error" or when the normal prefix is not logged but the result should be) 
    */ 
   def logState(prefix: String = logPrefix) {
      def logHistory(h: History) {
         logGroup {
            h.getSteps.reverse.foreach(s => report(prefix, s.present))
         }
      }
      logGroup {
         report(prefix, "unknowns: " + initUnknowns.toString)
         report(prefix, "solution: " + solution.toString)
         val unsolved = getUnsolvedVariables.map(_.name)
         if (! unsolved.isEmpty)
            report(prefix, "unsolved: " + unsolved.mkString(", "))
         else
            report(prefix, "all variables solved")
         if (! errors.isEmpty) {
            report(prefix, "errors:")
            logGroup {
               errors.foreach {e =>
                  report(prefix, "error: " + e.getSteps.head.present)
                  logHistory(e)
               }
            }
         } else
            report(prefix, "no errors")
         if (! delayed.isEmpty) {
            report(prefix, "constraints:")
            logGroup {
               delayed.foreach {
                  case d: DelayedJudgement =>
                     report(prefix, d.constraint.present)
                     logGroup {
                        report(prefix, d.constraint.presentAntecedent(_.toString))
                        report(prefix, d.constraint.presentSucceedent(_.toString))
                        if (errors.isEmpty)
                           // if there are no errors, see the history of the constraints
                           logHistory(d.history)
                     }
                  case d: DelayedInference =>
                     report(prefix, "continuation after delayed inference of  " + presentObj(d.tm))
                     if (errors.isEmpty)
                        logHistory(d.history)
               }
            }
         } else
            report(prefix, "no remaining constraints")
      }
   }
   
   /** retrieves the type type of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getType(p: GlobalName): Option[Term] = {
      val c = controller.globalLookup.getConstant(p)
      if (c.tpC.analyzed.isDefined) dependencies ::= p $ TypeComponent
      c.tpC.analyzed
   }
   /** retrieves the definiens of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getDef(p: GlobalName) : Option[Term] = {
      val c = controller.globalLookup.getConstant(p)
      if (c.dfC.analyzed.isDefined) dependencies ::= p $ DefComponent
      c.dfC.analyzed
   }

   /** registers the solution for an unknown variable
    * 
    * If a solution exists already, their equality is checked.
    * @param name the solved variable
    * @param value the solution; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the solution differs from an existing one
    * precondition: value is well-typed if the overall check succeeds
    */
   def solve(name: LocalName, value: Term)(implicit stack: Stack, history: History) : Boolean = {
      log("solving " + name + " as " + value)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined) {
         check(Equality(stack, value, solved.df.get, solved.tp))(history + "solution must be equal to previously found solution") //TODO
      } else {
         val valueS = simplify(value ^^ left.toPartialSubstitution) // substitute solutions of earlier variables that have been found already
         parser.SourceRef.delete(valueS) // source-references from looked-up types may sneak in here
         val rightS = right ^^ (OMV(name) / valueS) // substitute in solutions of later variables that have been found already
         val vd = solved.copy(df = Some(valueS))
         solution = left ::: vd :: rightS
         newsolutions ::= name
         typeCheckSolution(vd)
      }
   }
   /** registers the solved type for a variable
    * 
    * If a type exists already, their equality is checked.
    * @param name the variable
    * @param value the type; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the type differs from an existing one
    * precondition: value is well-typed if the the overall check succeeds
    */
   private def solveType(name: LocalName, value: Term)(implicit stack: Stack, history: History) : Boolean = {
      val valueS = simplify(value)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.tp.isDefined)
         check(Equality(stack, valueS, solved.tp.get, None))(history + "solution for type must be equal to previously found solution") //TODO
      else {
         val vd = solved.copy(tp = Some(valueS))
         solution = left ::: vd :: right
         //no need to register this in newsolutions
         typeCheckSolution(vd)
      }
   }
   /** if the type and the definiens of an unknown are solved independently, this type-checks them */
   private def typeCheckSolution(vd: VarDecl)(implicit stack: Stack, history: History): Boolean = {
      (vd.tp, vd.df) match {
         case (Some(tp), Some(df)) =>
            check(Typing(stack, df, tp))(history + "checking solution of metavariable against solved type")
         case _ => true
      }
   }
   
   /**
    * @param newVars new unknowns; creating new unknowns during checking permits variable transformations
    * @param before the variable before which to insert the new ones 
    */
   def addUnknowns(newVars: Context, before: LocalName): Boolean = {
      val (left, right) = solution.span(_.name != before)
      solution = left ::: newVars ::: right
      true
   }
   
   /** registers an error
    *  @return false
    */
   def error(message: => String)(implicit history: History): Boolean = {
      log("error: " + message)
      errors ::= history + message
      // maybe return true so that more errors are found
      false
   }
   
   /** applies this Solver to one Judgement
    *  This method can be called multiple times to solve a system of constraints.
    *  @param j the Judgement
    *  @return if false, j is disproved; if true, j holds relative to all delayed judgements and errors
    *  
    *  Note that this may return true even if can be disproved, namely if the delayed judgements are disproved later.
    *  
    *  If this returns false, an error must have been registered.  
    */
   
   def apply(j: Judgement): Boolean = {
      delayed ::= new DelayedJudgement(j, new History(Nil), true)
      activate
   }
   
   /** delays a constraint for future processing
    * @param j the Judgement to be delayed
    * @param activatable true if the judgment can be activated immediately
    * @return true (i.e., delayed Judgment's always return success)
    */
   private def delay(j: Judgement, incomplete: Boolean = false)(implicit history: History): Boolean = {
      // testing if the same judgement has been delayed already
      if (delayed.exists {
         case d: DelayedJudgement => d.constraint hasheq j
         case _ => false
      }) {
         log("delaying (exists already): " + j.present)
      } else {
         log("delaying: " + j.present)
         history += "(delayed)"
         val dc = new DelayedJudgement(j, history, incomplete)
         delayed ::= dc
      }
      true
   }

   /** true if there is an activatable constraint that is not incomplete */
   private def existsActivatable : Boolean = delayed.exists {d => d.isActivatable && !d.incomplete}
   
   /**
    * processes the next activatable constraint until none are left
    * 
    * if there is no activatable constraint, we try an incomplete constraint as a last resort
    */
   @scala.annotation.tailrec
   private def activate : Boolean = {
     val subs = solution.toPartialSubstitution
     def prepare(t: Term, covered: Boolean = false) = if (covered) simplify(t ^^ subs) else simplify(t ^^ subs)
     def prepareS(s: Stack) =
        Stack(s.frames.map(f => Frame(f.theory, controller.uom.simplifyContext(f.context ^^ subs, theory, solution))))
     // look for an activatable constraint
     delayed foreach {_.solved(newsolutions)}
     newsolutions = Nil
     val dcOpt = delayed.find {d => d.isActivatable && !d.incomplete} orElse {
         log("no activatable constraint, trying incomplete reasoning")
         delayed.find {_.incomplete}
     }
     dcOpt match {
        case None =>
           // no activatable constraint: return true if no errors (other constraints may be left)
           errors.isEmpty
        case Some(dc) =>
           // activate a constraint
           delayed = delayed filterNot (_ == dc)
           // mayhold is the result of checking the activated constraint
           val mayhold = dc match {
              case dj: DelayedJudgement =>
                 val j = dj.constraint
                 implicit val history = dj.history
                 //logState() // noticably slows down type-checking, only use for debugging
                 log("activating: " + j.present)
                 j match {
                    case Typing(stack, tm, tp, typS) =>
                       check(Typing(prepareS(stack), prepare(tm), prepare(tp, true), typS))
                    case Equality(stack, tm1, tm2, tp) =>
                       check(Equality(prepareS(stack), prepare(tm1, true), prepare(tm2, true), tp.map {x => prepare(x, true)}))
                    case Universe(stack, tm) =>
                       check(Universe(prepareS(stack), prepare(tm)))
                    case Inhabitable(stack, tm) =>
                       check(Inhabitable(prepareS(stack), prepare(tm)))
                    case IsMorphism(stack, mor, from) =>
                       checkMorphism(mor ^^ subs, prepare(from))(prepareS(stack), history)
                 }
              case di: DelayedInference =>
                  inferTypeAndThen(prepare(di.tm))(prepareS(di.stack), di.history)(di.cont)
           }
           if (mayhold) {
              //recurse to activate the next constraint
              activate
           } else
              // the judgment was disproved
              false
           }
   }

   def inferTypeAndThen(tm: Term)(stack: Stack, history: History)(cont: Term => Boolean): Boolean = {
      implicit val (s,h) = (stack, history)
      inferType(tm) match {
         case Some(tp) =>
            cont(tp)
         case None =>
            delayed ::= new DelayedInference(stack, history + "(inference delayed)", tm, cont)
            true
      }
   }
   
   /**
    * checks a judgement
    *  
    * This is the method that should be used to recurse into a hypothesis.
    * It handles logging and error reporting and delegates to specific methods based on the judgement.
    *  
    * The apply method is similar but additionally simplifies the judgment.
    * 
    * @param j the judgement
    * @return like apply
    */
   def check(j: Judgement)(implicit history: History): Boolean = {
      history += j
      log(j.presentSucceedent)
      logGroup {
         log(j.presentAntecedent)
         j match {
            case j: Typing   => checkTyping(j)
            case j: Equality => checkEquality(j)
            case j: Universe => checkUniverse(j)
            case j: Inhabitable => checkInhabitable(j)
         }
      }
   }
   
   /** proves a Typing Judgement by bidirectional type checking
    *  
    * In particular, uses TypingRule's to branch and InferenceRule's followed by Equality check as fallback.
    *  
    * pre: context and type are covered
    *
    * post: typing judgment is covered
    *  
    */
   private def checkTyping(j: Typing)(implicit history: History) : Boolean = {
     val tm = j.tm
     val tp = j.tp
     implicit val stack = j.stack
     tm match {
       // the foundation-independent cases
       case OMV(x) => (solution ++ stack.context)(x).tp match {
         case None =>
            if (solution.isDeclared(x))
              solveType(x, tp) //TODO: check for occurrences of bound variables?
            else
              error("untyped variable type-checks against nothing: " + x)
         case Some(t) => check(Equality(stack, t, tp, None))
       }
       case OMS(p) =>
         getType(p) match {
           case None => getDef(p) match {
             case None =>
                //TODO: we may want to check if there is a typing rule for p
                error("untyped, undefined constant type-checks against nothing: " + p.toString)
             case Some(d) => check(Typing(stack, d, tp, j.tpSymb)) // expand defined constant
           }
           case Some(t) => check(Equality(stack, t, tp, None))
         }
       case l: OMLIT => check(Equality(stack, OMS(l.rt.synType), tp, None))
       // the foundation-dependent cases
       // bidirectional type checking: first try to apply a typing rule (i.e., use the type early on), if that fails, infer the type and check equality
       case tm =>
         limitedSimplify(tp,ruleStore.typingRules) match {
           case (tpS, Some(rule)) =>
             rule(this)(tm, tpS)
           case (tpS, None) =>
             // either this is an atomic type, or no typing rule is known
             inferType(tm)(stack, history.branch) match {
               case Some(itp) =>
                  check(Equality(stack, itp, tpS, None))(history + ("inferred type must be equal to expected type; the term is: " + presentObj(tm)))
               case None =>
                  delay(Typing(stack, tm, tpS, j.tpSymb))(history + "type inference failed")
             }
         }
     }
   }

   /** infers the type of a term by applying InferenceRule's
    * @param tm the term
    * @param stack its Context
    * @return the inferred type, if inference succeeded
    *
    * This method should not be called by users (instead, use object Solver.check).
    * It is only public because it serves as a callback for Rule's.
    *
    * pre: context is covered
    *
    * post: if result, typing judgment is covered
    */
   def inferType(tm: Term)(implicit stack: Stack, history: History): Option[Term] = {
     log("inference: " + presentObj(tm) + " : ?")
     history += "inferring type of " + presentObj(tm)
     // return previously inferred type, if any (previously unsolved variables are substituted)
     InferredType.get(tm) match {
        case s @ Some(_) => return s.map(_ ^^ solution.toPartialSubstitution)
        case _ =>
     }
     val res = logGroup {
        log("in context: " + presentObj(stack.context))
        val resFoundInd = tm match {
          //foundation-independent cases
          case OMV(x) =>
             history += "lookup in context"
             (solution ++ stack.context)(x).tp
          case OMS(p) =>
             history += "lookup in theory"
             getType(p) orElse {
               getDef(p) match {
                 case None => None
                 case Some(d) => inferType(d) // expand defined constant
               }
             }
          case l: OMLIT =>
             history += "lookup in literal"
             // structurally well-formed literals carry their type
             return Some(OMS(l.rt.synType)) // no need to use InferredType.put on literals
          case _ => None
        }
        //foundation-dependent cases if necessary
        //syntax-driven type inference
        resFoundInd orElse {
            val (tmS, ruleOpt) = limitedSimplify(tm,ruleStore.inferenceRules)
            ruleOpt match {
              case Some(rule) =>
                 history += ("applying rule for " + rule.head.name.toString)
                 rule(this)(tmS)
              case None => None
            }
        }
     }
     log("inferred: " + res.getOrElse("failed"))
     // remember inferred type
     if (res.isDefined) InferredType.put(tm, res.get)
     res
   }

   /** proves a Universe Judgment
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   private def checkUniverse(j : Universe)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     limitedSimplify(j.univ, ruleStore.universeRules) match {
        case (uS, Some(rule)) => rule(this)(uS)
        case (uS, None) => delay(Universe(stack, uS))  
     }
   }
   
   /** proves an Inhabitable Judgment
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   private def checkInhabitable(j : Inhabitable)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     limitedSimplify(j.wfo, ruleStore.inhabitableRules) match {
        case (uS, Some(rule)) => rule(this)(uS)
        case (uS, None) =>
           inferType(j.wfo)(stack, history + "inferring universe") match {
             case None =>
                delay(Inhabitable(stack, uS))
             case Some(univ) =>
                check(Universe(stack, univ))
          }
     }
   }
   
   /** proves an Equality Judgment by recursively applying in particular EqualityRule's.
    * @param j the judgement
    * @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    * 
    * pre: context, terms, and (if given) type are covered; if type is given, both typing judgments are covered
    * 
    * post: equality is covered
    */
   private def checkEquality(j: Equality)(implicit history: History): Boolean = {
      val tm1 = j.t1
      val tm2 = j.t2
      val tpOpt = j.t
      implicit val stack = j.stack
      val tm1S = simplify(tm1)
      val tm2S = simplify(tm2)
      // 1) base cases, e.g., identical terms, solving unknowns
      // identical terms
      if (tm1S hasheq tm2S) return true
         // different literals are always non-equal
      (tm1S, tm2S) match {
         case (l1: OMLIT, l2: OMLIT) => if (l1.value != l2.value) return error(s"$l1 and $l2 are inequal literals")
         case _ =>
      }
      // solve an unknown
      val solved = solveEquality(tm1S, tm2S, tpOpt) || solveEquality(tm2S, tm1S, tpOpt)
      if (solved) return true
      
      // 2) find a TermBasedEqualityRule based on the heads of the terms
      (tm1.head, tm2.head) match {
         case (Some(c1), Some(c2)) =>
            ruleStore.termBasedEqualityRules(c1,c2) foreach {rule =>
               // we apply the first applicable rule
               val contOpt = rule(this)(tm1,tm2,tpOpt)
               if (contOpt.isDefined) {
                  return contOpt.get.apply
               }
            }
         case _ =>
      }

      // 3) find a TypeBasedEqualityRule based on the head of the type
      // first infer the type if it has not been given in tpOpt
      val tp = tpOpt match {
        case Some(tp) => tp
        case None =>
           val itp = inferType(tm1S) orElse inferType(tm2S)(stack, history + "inferring omitted type")
           itp.getOrElse(return delay(Equality(stack, tm1S, tm2S, None)))
      }
      // try to simplify the type until an equality rule is applicable 
      limitedSimplify(tp, ruleStore.typeBasedEqualityRules) match {
         case (tpS, Some(rule)) => rule(this)(tm1S, tm2S, tpS)
         case (tpS, None) =>
            // this is either a base type or an equality rule is missing
            checkEqualityTermBased(List(tm1S), List(tm2S), false)(stack, history, tp)
      }
   }
   
   /* ********************** auxiliary methods of checkEquality ***************************/ 
   
   /**
    * finds a TermBasedEqualityRule that applies to t1 and any of the terms in t2
    * rules are looked up based on the torsos of the terms
    * first found rule is returned
    */
   private def findEqRule(t1: Term, others: List[Term])(implicit stack : Stack, history: History, tp: Term) : Option[Continue[Boolean]] = {
      t1 match {
         case TorsoNormalForm(OMS(c1), _) => others foreach {o => o match {
            case TorsoNormalForm(OMS(c2), _) => 
               ruleStore.termBasedEqualityRules(c1,c2) foreach {rule =>
                  val contOpt = rule(this)(t1, o, Some(tp))
                  if (contOpt.isDefined) return contOpt
               }
            case _ =>
         }}
         case _ =>
      }
      return None
   }
   
   /**
    * checks equality t1 = t2 at an atomic type by simplification
    * 
    * Both t1 and t2 are remembered as the chain of terms resulting from repeated definition expansion 
    * @param terms1 the chain of terms that led to t1 (must be non-empty)
    * @param terms2 the chain of terms that led to t2 (must be non-empty)
    * @param t2Final if true, no simplification possible anymore
    * @param flipped if true, t1 and t2 have been flipped during recursive calls (false by default)
    *   This parameter is not semantically necessary but carried around to undo flipping when calling other functions.
    * @return true if equal
    */
   private def checkEqualityTermBased(terms1: List[Term], terms2: List[Term], t2Final: Boolean, flipped: Boolean = false)(implicit stack : Stack, history: History, tp: Term) : Boolean = {
       log("equality (trying rewriting): " + terms1.head + " = " + terms2.head)
       val t1 = terms1.head
       // see if we can expand a definition in t1
       val t1E = defExp(t1, Context())
       val t1EL = limitedSimplify(t1E)(_ => None)._1
       val t1ES = simplify(t1EL)
       val changed = t1ES hashneq t1
       if (changed) {
          log("left term rewritten to " + t1ES)
          // check if it is identical to one of the terms known to be equal to t2
          if (terms2.exists(_ hasheq t1ES)) {
             log("success by identity")
             return true
          }
          // check if there is a TermBasedEqualityRule that applies to the new torso of t1 and the torso of a term equal to t2 
          val contOpt = findEqRule(t1ES, terms2)
          contOpt foreach {cont => return cont.apply()}
       }
       // if we failed to prove equality, we have multiple options:
       (changed, t2Final) match {
          // t1 expanded, t2 cannot be expanded anymore --> keep expanding t1
          case (true, true)      => checkEqualityTermBased(t1ES::terms1, terms2, true, flipped)
          // t1 expanded, t2 can still be expanded anymore --> continue expanding t2
          case (true, false)     => checkEqualityTermBased(terms2, t1ES::terms2, false, ! flipped)
          // t1 cannot be expanded anymore but t2 can ---> continue expanding t2
          case (false, false)    => checkEqualityTermBased(terms2, terms1, true, ! flipped)
          // neither term can be expanded --> try congruence rule as last resort
          case (false, true)     =>
             // if necessary, we undo the flipping of t1 and t2
             val s1 = if (flipped) terms2.head else t1ES
             val s2 = if (flipped) t1ES else terms2.head
             if (existsActivatable)
                // if there is some other way to proceed, do it
                delay(Equality(stack, s1, s2, Some(tp)), true)
             else
                // last resort try congruence
                checkEqualityCongruence(TorsoForm.fromHeadForm(s1), TorsoForm.fromHeadForm(s2))
       }
   }

   /**
    * tries to prove equality using basic congruence rule
    * torso and heads must be identical, checkEquality is called in remaining cases 
    */
   private def checkEqualityCongruence(t1: TorsoForm, t2: TorsoForm)(implicit stack : Stack, history: History, tp: Term): Boolean = {
      lazy val j = Equality(stack, t1.toHeadForm, t2.toHeadForm, Some(tp))
      lazy val noFreeVars = j.freeVars.isEmpty
      /* The torso form focuses on sequences of elimination operators to the same torso.
       * If the heads of the terms are introduction operators h, the congruence rule can usually be applied greedily
       * because introduction is usually injective.
       * For that purpose, a TermBasedEqualityRule(h,h) should be provided.
       * Therefore, a congruence rule for binders is currently not necessary.
       */
      val similar = t1.torso == t2.torso && t1.apps.length == t2.apps.length
      if (similar) {
         (t1.apps zip t2.apps).forall {case (Appendage(head1,args1),Appendage(head2,args2)) =>
             val similarApp = head1 == head2 && args1.length == args2.length
             if (! similarApp) {
                if (noFreeVars)
                   error("terms have different shape, thus they cannot be equal")
                else
                   delay(j)
             } else {
                log("equality (trying congruence)")
                history += "applying congruence"
                similarApp && (args1 zip args2).forall {case (a1, a2) =>
                   check(Equality(stack, a1, a2, None))(history + "comparing argument")
                }
             }
         }
      } else {
         if (noFreeVars)
            error("terms have different shape, thus they cannot be equal")
         else
            //TODO can we fail if there are free variables but only in the context?
            delay(j)
      }
   }
   

   /** tries to solve an unknown occurring as the torso of tm1 in terms of tm2.
    * 
    * This method should not be called by users (instead, use check(Equality(tm1,tm2,...))).
    * It is only public because it serves as a callback for SolutionRule's.
    * (SolutionRule's should not recurse into check(Equality(...)) because it tends to lead to cycles.)
    */
   def solveEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit stack: Stack, history: History): Boolean = {
      tm1 match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) =>
            val mIndex = solution.index(m).getOrElse {
               // if m is not declared in unknowns, i.e., is a bound variable, nothing to do 
               return false
            }
            // fvsEarlier: all unknowns in tm2 are declared before m
            val fvs = tm2.freeVars
            val fvsEarlier = fvs.forall {v =>
               solution.index(v) match {
                  case Some(vIndex) => vIndex > mIndex
                  case None => false
               }
            }
            if (fvsEarlier) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solve(m, tm2)
               tpOpt foreach {case tp => solveType(m, tp)}
               res
            } else
               delay(Equality(stack,tm1,tm2,tpOpt))
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // note: tm2 should be simplified - that may make a free variable disappear
         //apply a foundation-dependent solving rule selected by the head of tm1
         case TorsoNormalForm(OMV(m), Appendage(h,_) :: _) if solution.isDeclared(m) && ! tm2.freeVars.contains(m) => //TODO what about occurrences of m in tm1?
            ruleStore.solutionRules.get(h) match {
               case None => false
               case Some(rule) => rule(this)(tm1, tm2)
            }
         case _ => false
      }
   }
   /* ********************** end of auxiliary methods of checkEquality ***************************/
   
   private def checkMorphism(mor: Term, from : Term)(implicit stack: Stack, history: History) : Boolean = {
     log("typing: " + mor + " : " + from)
     logGroup {
        log("in context: " + stack.context)
        from match {
          case OMMOD(p) => controller.globalLookup.getTheory(p) match {
            case thdf : DefinedTheory =>  checkMorphism(mor, thdf.df)
            case thd : DeclaredTheory =>
              val clist : List[Declaration] = thd.getDeclarations filter (p => !p.isInstanceOf[Structure])  // list of constants in the domain theory
              false //TODO
          }
          case _ => false
        }
     }
   }

   /** applies all ForwardSolutionRules of the given priority
    * @param priority exactly the rules with this Priority are applied */
   //TODO call this method at appropriate times
   private def forwardRules(priority: ForwardSolutionRule.Priority)(implicit stack: Stack, history: History): Boolean = {
      val results = solution.zipWithIndex map {
         case (vd @ VarDecl(x, Some(tp), None), i) =>
            implicit val con : Context = solution.take(i)
            limitedSimplify(tp, ruleStore.forwardSolutionRules) match {
               case (tpS, Some(rule)) if rule.priority == priority => rule(this)(vd)
               case _ => false 
            }
         case _ => false
      }
      results.exists(_ == true)
   }
   
   /** special case of the version below where we simplify until an applicable rule is found
    *  @param tm the term to simplify
    *  @param rm the RuleMap from which an applicable rule is needed
    *  @param stack the context of tm
    *  @return (tmS, Some(r)) where tmS = tm and r from rm is applicable to tmS; (tmS, None) if tm = tmS and no further simplification rules are applicable
    */  
   private def limitedSimplify[R <: Rule](tm: Term, rm: RuleMap[R])(implicit stack: Stack, history: History): (Term,Option[R]) =
      limitedSimplify[R](tm)(t => t.head flatMap {h => rm.get(h)})
   
   /** applies ComputationRule's to simplify a term until some condition is satisfied;
    *  A typical case is transformation into weak head normal form.
    *  @param tm the term to simplify (It may be simple already.)
    *  @param simple a term is considered simple if this function returns a non-None result
    *  @param stack the context of tm
    *  @return (tmS, Some(a)) if tmS is simple and simple(tm)=tmS; (tmS, None) if tmS is not simple but no further simplification rules are applicable
    */
   def limitedSimplify[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term,Option[A]) = {
      simple(tm) match {
         case Some(a) => (tm,Some(a))
         case None => tm.head match {
            case Some(h) =>
               val rOpt = ruleStore.computationRules.get(h)
               rOpt foreach {rule =>
                  rule(this)(tm) match {
                     case Some(tmS) =>
                        log("simplified: " + tm + " ~~> " + tmS)
                        return limitedSimplify(tmS.from(tm))(simple)
                     case None =>
                  }
               }
               // no rule or rule not applicable, expand a definition
               val tmE = defExp(tm,Context())
               if (tmE hashneq tm) limitedSimplify(tmE)(simple)
               else (tmE, None)
            case None => (tm, None)
         }
      }
   }
}

object Solver {
  /** reconstructs a single term and returns the reconstructed term and its type */
  def check(controller: Controller, stack: Stack, tm: Term): Option[(Term,Term)] = {
      val (unknowns,tmU) = parser.AbstractObjectParser.splitOffUnknowns(tm)
      val etp = LocalName("expected_type")
      val oc = new Solver(controller, stack.theory, unknowns ++ VarDecl(etp, None, None))
      val j = Typing(stack, tmU, OMV(etp), None)
      oc(j)
      if (oc.checkSucceeded) oc.getSolution.map {sub =>
          val tmR = tmU ^ sub
          (tmR, sub("expected_type").get) // must be defined if there is a solution
      } else
         None
  }
}