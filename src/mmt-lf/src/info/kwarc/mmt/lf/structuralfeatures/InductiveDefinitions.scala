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
import StructuralFeatureUtils._
import TermConstructingFeatureUtil._
import inductiveUtil._

/** theories as a set of types of expressions */ 
class InductiveDefinitions extends StructuralFeature("inductive_definition") with TypedParametricTheoryLike {
  
  /**
   * Checks the validity of the inductive type(s) to be constructed
   * @param dd the derived declaration from which the inductive type(s) are to be constructed
   */
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {}
  

  /**
   * Elaborates an declaration of one or multiple mutual inductive types into their declaration, 
   * as well as the corresponding no confusion and no junk axioms
   * Constructs a structure whose models are exactly the (not necessarily initial) models of the declared inductive types
   * @param parent The parent module of the declared inductive types
   * @param dd the derived declaration to be elaborated
   */
  def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration) = {
    val (indDefPath, context, indParams) = ParamType.getParams(dd)
    val (indD, indCtx) = controller.library.get(indDefPath) match {
      case indD: DerivedDeclaration if (indD.feature == "inductive") => (indD, Type.getParameters(indD))
      case d: DerivedDeclaration => throw LocalError("the referenced derived declaration is not of the feature inductive but of the feature "+d.feature+".")
      case _ => throw LocalError("Expected definition of corresponding inductively-defined types at "+indDefPath.toString()
            +" but no derived declaration found at that location.")
    }
    //The internal definitions we need to give definiens for
    val intDecls = parseInternalDeclarations(indD, controller, None)(indD.path) filterNot (_.isOutgoing)
    var (tpls, tmls) = (InternalDeclaration.tpls(intDecls), InternalDeclaration.tmls(intDecls))
    
    val (tplNames, tmlNames, intDeclNames) = (tpls map (_.name), tmls map (_.name), intDecls map (_.name))
    
    val defDecls = parseInternalDeclarationsWithDefiniens(dd, controller, Some(context), indD.path)
    val (defTpls, defConstrs) = (InternalDeclaration.tpls(defDecls), constrs(defDecls))
    
    // check whether all declarations match their corresponding constructors
    defDecls foreach { 
      d => correspondingDecl(indD, d.name) map { decl => 
      checkDecl(d, fromConstant(decl, controller, tpls.map(_.path), None)(indD.path))
      }
    }
    // and whether we have all necessary declarations
    indD.getDeclarations.map(_.name).find(n => !defDecls.map(_.name).contains(n)) foreach {
      n => throw LocalError("No declaration found for the internal declaration "+n+" of "+indD.name+".")
    }
    
    val induct_paths = intDecls map (t=>t.path.copy(name=indD.name/inductName(t.name)))
    
    val modelDf = defDecls map (_.df.get)
    val intDeclsArgs = intDecls map(_.argContext(None)(indD.path)._1)
    
    // Correctly sorted version of decls
    val defDeclsDef = intDeclNames map (nm => defDecls.find(_.name == nm).getOrElse(
        throw LocalError("No declaration found for the typelevel: "+nm)))
    val Tps = intDecls zip defDeclsDef map {
      case (tpl, tplDef) if tpl.isTypeLevel => PiOrEmpty(context, Arrow(tpl.toTerm(indD.path), tplDef.df.get))
      case (tml: Constructor, tmlDef) => 
        val tpl = tml.getTpl(tpls)
        val tplDef = defDecls.find(_.name == tpl.name).get
        val (args, dApplied) = tml.argContext(None)(indD.path)
        PiOrEmpty(context++args, Eq(tplDef.df.get, tpl.applyTo(dApplied)(dd.path), tmlDef.df.get))
    }
    val Dfs = intDecls zip intDeclsArgs zip induct_paths.map(OMS(_)) map {case ((intDecl, intDeclArgs), tm) => 
        PiOrEmpty(context++intDeclArgs, ApplyGeneral(tm, indParams++modelDf++intDeclArgs.map(_.toTerm)))
    }
    
    var inductDefs = (intDecls zip Tps zip Dfs) map {case ((tpl, tp), df) => 
      makeConst(tpl.name, ()=> {tp}, false,() => {Some(df)})(dd.path)}
    inductDefs map(d => println(defaultPresenter(d)(controller)))
    
    // Add a more convenient version of the declarations for the tmls by adding application to the function arguments via a chain of Congs
    val inductTmlsApplied = defDeclsDef zip (Tps zip Dfs) filter(_._1.isConstructor) map {
      case (d: Constructor, (tp, df)) =>
        val defTplDef = d.getTpl(defTpls).df.get
        val ags = defTplDef match {case FunType(ags, rt) => if (ags.isEmpty) List((None, rt)) else ags.tail.+:(None,rt)}
        val args = ags.zipWithIndex map {case ((n, t), i) => (n.map(_.toString()).getOrElse("x_"+i), t)}
        val argsCtx = args map {case (n, tp) => newVar(n, tp, d.ctx)}
        
        val inductDefApplied = Congs(tp, df, argsCtx)
        makeConst(appliedName(d.name), () => {PiOrEmpty(argsCtx,inductDefApplied._1)}, false, () => Some(Lambda(argsCtx,inductDefApplied._2)))(dd.path)
    }

    inductDefs ++ inductTmlsApplied
    
    new Elaboration {
      def domain = inductDefs map (_.name)
      def getO(n: LocalName) = {
        inductDefs find (_.name == n) foreach(d=>log(defaultPresenter(d)(controller)))
        inductDefs find (_.name == n)
      }
    }
  }
  
  /**
   * checks whether d.tp matches the type decl.externalTp
   * @param d the declaration whoose type to check
   * @param decl the corresponding declaration which is to be defined by d
   * @note this will return an error if d.tp doesn't match decl.externalTp
   */
  def checkDecl(d: InternalDeclaration, decl: InternalDeclaration) {
    //TODO: This should be implemented to provide more accurate error messages
    //TODO: It should set the tp.analize fields of the internal declarations to the expected types (and definitions if any)
  }
}

object InductiveDefinitionRule extends StructuralFeatureRule(classOf[InductiveDefinitions], "inductive_definition")