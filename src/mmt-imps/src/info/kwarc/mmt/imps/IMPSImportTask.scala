package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque.{OpaqueText, StringFragment}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.imps.Usage.Usage

import info.kwarc.mmt.lf.{Apply, ApplySpine}
import utils._

/* REMINDER:

DPath = Namespace
LocalName(s : String)
Dpath ? LocalName = MPath (theory/view)
MPath ? LocalName = GlobalName (declarations)

namespace http://imps.blubb

theory Booleans =
 constant true%val <- http://imps.blubb?Booleans?true%val
 */

class IMPSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit, tState : TranslationState)
  extends Logger with MMTTask
{
	          def logPrefix : String = "imps-omdoc"
	protected def report    : Report = controller.report

  val rootdpath : DPath                = DPath(URI.http colon "imps.mcmaster.ca") /* arbitrary, but seemed fitting */

  /* Source References. Methods exceedingly small, but look nicer than doing this everywhere directly */
  def doSourceRef(t : Term, s : SourceRef)        : Unit = { SourceRef.update(t, s) }
  def doSourceRef(d : Declaration, s : SourceRef) : Unit = { SourceRef.update(d, s) }

  /* Add metadata from usages element */
  def doUsages(d : Declaration, usages : List[Usage]) : Unit =
  {
    for (usage <- usages)
    {
      // using rootdpath and not IMPSTheory.rootdpath because this is IMPS, not LUTINS
      val metadata_verb   : GlobalName = rootdpath ? d.name ? LocalName("usage")
      val metadata_object : Obj        = OMS(rootdpath ? d.name ? usage.toString)
      d.metadata.add(new MetaDatum(metadata_verb, metadata_object))
    }
  }

  /* Add more generic metadata, meant to be used for short strings, not proofs
   * Might be rewritten, when we have cleverer solutions for MetaData */
  def doMetaData(d : Declaration, metaVerb : String, metaObject : String) : Unit =
  {
    val mv : GlobalName =     rootdpath ? d.name ? LocalName(metaVerb)
    val mo : Obj        = OMS(rootdpath ? d.name ? metaObject)

    d.metadata.add(new MetaDatum(mv,mo))
  }

  def doName(s : String) : LocalName = LocalName(s)

	def doDocument(es : Exp, uri : URI) : BuildResult =
	{
    val doc = new Document(bt.narrationDPath, true)
    controller.add(doc)

    for (exp <- es.children)
    {
      exp match
      {
        /* Translating Theories to MMT */
        case t@(Theory(_,_,_,_,_,_))       => doTheory(t)
        // Languages are processed in context of theories using them, not by themselves
        case l@(Language(_,_,_,_,_,_,_,_)) => tState.languages = tState.languages :+ l
        // If it's none of these, fall back to doDeclaration
        case _                             => doDeclaration(exp)
      }
    }

    // Run Checker (to resolve unknowns, etc)
    // Set to true to run
    val typecheck : Boolean = true

    if (typecheck)
    {
      log("Checking:")
      logGroup
      {
        val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
          throw GeneralError("no checker found")
        }.asInstanceOf[MMTStructureChecker]
        tState.theories_decl foreach { p =>
          val ce = new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore, this)
          checker.apply(p)(ce)
        }
      }
    }

    index(doc)
    BuildSuccess(Nil,Nil)
	}

  def doTheory (t : Theory) : Unit =
  {
    val nu_theory = new DeclaredTheory(bt.narrationDPath,
                                       LocalName(t.name),
                                       Some(IMPSTheory.lutinsPath),
                                       modules.Theory.noParams,
                                       modules.Theory.noBase)


    tState.theories_decl = tState.theories_decl :+ nu_theory
    tState.theories_raw  = tState.theories_raw  :+ t

    controller.add(nu_theory)
    controller.add(MRef(bt.narrationDPath,nu_theory.path))

    /* Translate language of the theory */
    var l : Option[Language] = None

    // Build correct union of languages
    if (t.lang.isDefined) {
      assert(tState.languages.exists(la => la.name == t.lang.get.lang))
      l = tState.languages.find(la => la.name == t.lang.get.lang)
    }

    if (t.cmpntthrs.isDefined)
    {
      /* For each component theory, take its language (if there is one) */
      for (comp_theory <- t.cmpntthrs.get.lst)
      {
        assert(tState.theories_raw.exists(thy => thy.name == comp_theory))
        val t_index : Theory = tState.theories_raw.find(thy => thy.name == comp_theory).get

        if (t_index.lang.isDefined)
        {
          assert(tState.languages.exists(la => la.name == t_index.lang.get.lang))
          val l_prime: Language = tState.languages.find(la => la.name == t_index.lang.get.lang).get

          if (l.isDefined) {
            l = Some(l.get.union(l_prime))
          } else {
            l = Some(l_prime)
          }
        }
      }
    }

    // Actually translate resulting language
    if (l.isDefined) { doLanguage(l.get, nu_theory) }

    /* Translate all axioms, if there are any */
    if (t.axioms.isDefined)
    {
      var axiom_count : Int = -1
      for (ax <- t.axioms.get.axs)
      {
        val mth : Term = IMPSTheory.Thm(doMathExp(ax.formula, nu_theory))

        val name : String = if (ax.name.isDefined) { ax.name.get }
        else { axiom_count += 1 ; t.name + "_unnamed_axiom" + axiom_count.toString }

        val assumption = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))

        if (ax.usgs.isDefined) { doUsages(assumption,ax.usgs.get) }
        doSourceRef(assumption,ax.src)
        controller.add(assumption)
      }
    }

    /* All constants here per distinction element are
       axiomatically distinct from each other */
    if (t.dstnct.isDefined)
    {
      var dist_count : Int = 0

      for (dist : List[String] <- t.dstnct.get.lst) /* Reminder: t.dstnct.get.lst : List[List[String]] */
      {
        for (c1 : String <- dist)
        {
          for (c2 : String <- dist.filter(e => e != c1))
          {
            /* Assert the two constants to be distinct exist in the theory */
            assert(nu_theory.getDeclarations.exists(d => d.name == LocalName(c1)))
            assert(nu_theory.getDeclarations.exists(d => d.name == LocalName(c2)))

            /* add axiom that they are indeed distinct */
            val g1 : GlobalName = nu_theory.getDeclarations.find(d => d.name == LocalName(c1)).get.path
            val g2 : GlobalName = nu_theory.getDeclarations.find(d => d.name == LocalName(c2)).get.path

            val dist_formula : IMPSMathExp = IMPSNegation(IMPSEquals(IMPSSymbolRef(g1), IMPSSymbolRef(g2)))
            val mth          : Term = IMPSTheory.Thm(doMathExp(dist_formula, nu_theory))
            val name         : String = t.name + "_distinction_axiom_" + dist_count.toString

            dist_count += 1

            val dist_assumption = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))
            doSourceRef(dist_assumption, t.dstnct.get.src)
            controller.add(dist_assumption)
          }
        }
      }
    }
  }

  def doLanguage(l : Language, t : DeclaredTheory) : Unit =
  {
    def doLanguageOrTheory(target : String, t : DeclaredTheory) : Unit =
    {
      val exists_theory   : Boolean = tState.theories_raw.exists(p => p.name == target)
      val exists_language : Boolean = tState.languages.exists(p => p.name == target)
      assert(exists_language || exists_theory)

      if (exists_language)
      {
        doLanguage(tState.languages.find(p => p.name == target).get, t)
      }
      else if (exists_theory)
      {
        val argt : ArgumentLanguage = tState.theories_raw.find(p => p.name == target).get.lang.get
        assert(tState.languages.exists(p => p.name == argt.lang))
        doLanguage(tState.languages.find(p => p.name == argt.lang).get, t)
      }
    }

    if (l.embedlang.isDefined) {
      doLanguageOrTheory(l.embedlang.get.name,t)
    }

    if (l.embedlangs.isDefined)
    {
      for (l_embed <- l.embedlangs.get.names) {
        doLanguageOrTheory(l_embed, t)
      }
    }

    if (l.bstps.isDefined)
    {
      for (baseType : String <- l.bstps.get.tps)
      {
        val sort_type : Term = tState.addUnknown()
        tState.bindUnknowns(sort_type)

        val basetype = symbols.Constant(t.toTerm, doName(baseType), Nil, Some(sort_type), None, Some("BaseType"))
        doSourceRef(basetype, l.bstps.get.src)
        controller add basetype
      }
    }

    if (l.srts.isDefined)
    {
      /* introduce all sorts with their respective enclosing sorts */
      for (spec : (IMPSSort, IMPSSort) <- l.srts.get.lst)
        { doSubsort(spec._1, spec._2, t, l.srts.get.src) }
    }

    if (l.extens.isDefined)
    {
      for (tal : TypeSortAList <- l.extens.get.lst)
      {
        // TODO: Can this be translated into something non-opaque?
        //       See IMPS manual, pgs. 172, 173
        val opaque = new OpaqueText(t.path.toDPath, OpaqueText.defaultFormat, StringFragment(tal.toString))
        controller.add(opaque)
      }
    }

    if (l.cnstnts.isDefined)
    {
      for (pair : (String, IMPSSort) <- l.cnstnts.get.lst)
      {
        val mth_tp : Term = doSort(pair._2, t)
        val l_const = symbols.Constant(t.toTerm,doName(pair._1),Nil,Some(mth_tp),None,Some("Constant"))
        doSourceRef(l_const,l.cnstnts.get.src)
        controller add l_const
      }
    }
  }

  def doDeclaration (d : LispExp) : Unit =
  {
    // set this to true for helpful debug output
    val debug : Boolean = false
    if (debug)
    {
      println("\n>>>>> Call to doDecl for the following expression:\n")
      println(d.toString)
      for (thy <- tState.theories_decl)
      {
        println("\n<<<<< Theory " + thy.name + " contains the following declarations:")
        for(d <- thy.getDeclarations)
        { println("~~~ " + d.name.toString) }
      }
    }

    d match
    {
      case AtomicSort(name, defstring, theory, usages, witness, src) =>

        val ln : LocalName = LocalName(theory.thy)
        assert(tState.theories_decl.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent : DeclaredTheory = tState.theories_decl.find(dt => dt.name == ln).get

        val definition : Term = doMathExp(defstring, parent)
        val sort_type  : Term = tState.addUnknown()
        tState.bindUnknowns(sort_type)

        val nu_atomicSort = symbols.Constant(parent.toTerm, doName(name), Nil, Some(sort_type), Some(definition), Some("AtomicSort"))

        /* Add available MetaData */
        if (witness.isDefined) { doMetaData(nu_atomicSort, "witness", witness.get.witness.toString) }
        if (usages.isDefined)  { doUsages(nu_atomicSort, usages.get.usgs) }
        doSourceRef(nu_atomicSort,src)

        println("Adding atomic sort: " + name)
        controller add nu_atomicSort

      case Constant(name, definition, theory, sort, usages, src) =>

        val ln : LocalName = LocalName(theory.thy)
        assert(tState.theories_decl.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent : DeclaredTheory = tState.theories_decl.find(dt => dt.name == ln).get

        /* look for sort in given theory. */
        var srt : Option[Term] = None
        if (sort.isDefined)
        {
          println("Adding constant with clear sort: " + name)
          /* Theory not in scope, so we find it by hand */
          val theTheory : Option[DeclaredTheory] = tState.theories_decl.find(x => x.name == LocalName(theory.thy))
          assert(theTheory.isDefined)

          srt = Some(doSort(sort.get.sort, theTheory.get))
        }
        else {
          println("Adding constant with inferred sort: " + name)
          val sortx : Term = tState.addUnknown()
          tState.bindUnknowns(sortx)
          srt = Some(sortx)
        }

        val mth : Term = doMathExp(definition, parent)
        val nu_constant = symbols.Constant(parent.toTerm, LocalName(name), Nil, srt, Some(mth), Some("Constant"))

        /* Add available MetaData */
        doSourceRef(nu_constant,src)
        if (usages.isDefined) { doUsages(nu_constant,usages.get.usgs) }
        controller add nu_constant

      case Theorem(name, formula, lemma, reverse, theory, usages, transport, macete, homeTheory, maybeProof, src) =>

        val ln : LocalName = doName(theory.thy)
        assert(tState.theories_decl.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent : DeclaredTheory = tState.theories_decl.find(dt => dt.name == ln).get

        val mth : Term = IMPSTheory.Thm(doMathExp(formula, parent))
        val nu_theorem = symbols.Constant(parent.toTerm, doName(name), Nil, Some(mth), None, Some("Theorem"))
        //                                                                              ^-- proof goes here!

        /* Add available MetaData */
        if (usages.isDefined)     { doUsages(nu_theorem, usages.get.usgs) }
        if (transport.isDefined)  { doMetaData(nu_theorem, "translation", transport.get.trans) }
        if (macete.isDefined)     { doMetaData(nu_theorem, "macete", macete.get.macete) }
        if (homeTheory.isDefined) { doMetaData(nu_theorem, "homeTheory", homeTheory.get.hmthy) }

        if (lemma)   { doMetaData(nu_theorem,"lemma","present") }   else { doMetaData(nu_theorem,"lemma","absent") }
        if (reverse) { doMetaData(nu_theorem,"reverse","present") } else { doMetaData(nu_theorem,"reverse","absent") }

        doSourceRef(nu_theorem, src)
        controller add nu_theorem

        if (maybeProof.isDefined)
        {
          /* opaque proofs are beetter than no proofs */
          val proof_name : StringFragment = StringFragment("Opaque proof of theorem " + name)
          val proof_text : StringFragment = StringFragment(maybeProof.get.prf.toString)

          val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(proof_name + "\n" + proof_text))
          controller add opaque
        }

      case SchematicMacete(_, _, thy, _, _, _) =>

        val ln : LocalName = LocalName(thy.thy)
        assert(tState.theories_decl.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent : DeclaredTheory = tState.theories_decl.find(dt => dt.name == ln).get

        // Macetes are added as opaque (for now?)
        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))

        /* Opaque Text doesn't have metadata, apparently, so we don't add the src */

        controller add opaque

      case _ => log("Error: Unknown LispExp encountered, not translated!")
    }
  }

  def isProposition(m : IMPSMathExp) : Boolean =
  {
    m match
    {
      /* Look at that fancy fallthrough syntax */
      case IMPSTruth()
           | IMPSFalsehood()
           | IMPSConjunction(_)
           | IMPSDisjunction(_)
           | IMPSImplication(_,_)
           | IMPSIfForm(_,_,_)
           | IMPSIotaP(_,_,_)   => true
      case IMPSLambda(_,t)      => isProposition(t)
      case IMPSForAll(_,t)      => isProposition(t)
      case IMPSForSome(_,t)     => isProposition(t)
      case _                    => false
    }
  }

  def findType(s : IMPSSort) : Term =
  {
    s match {
      case IMPSAtomSort("ind")      => OMS(IMPSTheory.lutinsIndType)
      case IMPSAtomSort("prop")     => OMS(IMPSTheory.lutinsPropType)
      case IMPSAtomSort("bool")     => OMS(IMPSTheory.lutinsPropType)
      case IMPSAtomSort(_)          => OMS(IMPSTheory.lutinsIndType)
      case IMPSBinaryFunSort(s1,s2) => IMPSTheory.FunType(findType(s1),findType(s2))
      case _ => ??? // This should never happen, always call curry first!
    }
  }

  def doSort(d : IMPSSort, t : DeclaredTheory) : Term =
  {
    /* Walks sort structure, currying all NaryFunSorts into BinaryFunSorts */
    def curry(srt : IMPSSort) : IMPSSort =
    {
      srt match
      {
        case IMPSAtomSort(_)                 => srt // don't change atomic sorts
        case IMPSBinaryFunSort(sort1, sort2) => IMPSBinaryFunSort(curry(sort1),curry(sort2))
        case IMPSNaryFunSort(sorts)          => {
          if (sorts.length == 2) {
            IMPSBinaryFunSort(curry(sorts(0)), curry(sorts(1)))
          } else {
            IMPSBinaryFunSort(curry(sorts(0)), curry(IMPSNaryFunSort(sorts.tail)))
          }
        }
      }
    }

    def matchSort(e : IMPSSort, t : DeclaredTheory) : Term =
    {
      e match {
        case IMPSAtomSort("ind")  => IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType))
        case IMPSAtomSort("prop") => IMPSTheory.Sort(OMS(IMPSTheory.lutinsPropType))
        case IMPSAtomSort("bool") => IMPSTheory.Sort(OMS(IMPSTheory.lutinsPropType))
        case IMPSAtomSort(srt) => OMS(t.path ? srt)
        case IMPSBinaryFunSort(s1, s2) => {
          val tpA: Term = findType(s1)
          val tpB: Term = findType(s2)

          val sortA: Term = matchSort(s1, t)
          val sortB: Term = matchSort(s2, t)

          IMPSTheory.FunSort(tpA, tpB, sortA, sortB)
        }
      }
    }

    IMPSTheory.exp(matchSort(curry(d),t))
  }

  /* Introduces a sort to a theory and also assigns the enclosing sort to it. */
  def doSubsort(subsort : IMPSSort, supersort : IMPSSort, thy : DeclaredTheory, src : SourceRef) : Unit =
  {
    /* enclosing sort should already be defined */
    println("Adding sort: " + subsort.toString + ", enclosed by " + supersort.toString)

    val opt_ind   : Option[Term] = Some(Apply(OMS(IMPSTheory.lutinsPath ? LocalName("sort")), OMS(IMPSTheory.lutinsIndType)))
    val jdgmtname : LocalName    = LocalName(subsort.toString + "_sub_" + supersort.toString)

    val foo       : Term = doSort(subsort,thy)
    val bar       : Term = doSort(supersort,thy)
    val baz       : Term = OMS(IMPSTheory.lutinsIndType)

    val subs      : Term = ApplySpine(OMS(IMPSTheory.lutinsPath ? LocalName("subsort")), baz, foo, bar)

    val jdgmttp   : Option[Term] = Some(Apply(OMS(IMPSTheory.lutinsPath ? LocalName("thm")), subs))

    /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

    val typing    : Declaration  = symbols.Constant(thy.toTerm,LocalName(subsort.toString),Nil,opt_ind,None,Some("Subsort_1"))
    val judgement : Declaration  = symbols.Constant(thy.toTerm,jdgmtname,Nil,jdgmttp,None,Some("Subsort_2"))

    doSourceRef(typing,src)
    doSourceRef(judgement,src)

    controller add typing
    controller add judgement
  }

  /* Translate IMPS Math Expressions to Terms */
  def doMathExp(d : IMPSMathExp, thy : DeclaredTheory) : Term =
  {
    d match
    {
      case IMPSVar(v)             => OMV(v)

      case IMPSMathSymbol(s)      => OMS(thy.path ? LocalName(s))

      case q@IMPSTruth()          => OMS(IMPSTheory.lutinsPath ? "thetrue")
      case q@IMPSFalsehood()      => OMS(IMPSTheory.lutinsPath ? "thefalse")

      case IMPSNegation(p)        => IMPSTheory.Negation(doMathExp(p,thy))

      case IMPSIf(p,t1,t2)        => IMPSTheory.If(doMathExp(p,thy), doMathExp(t1,thy), doMathExp(t2,thy))
      case IMPSIff(p, q)          => IMPSTheory.Iff(doMathExp(p,thy), doMathExp(q,thy))
      case IMPSIfForm(p,q,r)      => IMPSTheory.If_Form(doMathExp(p,thy), doMathExp(q,thy), doMathExp(r,thy))
      case IMPSEquals(a,b)        => IMPSTheory.Equals(doMathExp(a,thy),doMathExp(b,thy))
      case IMPSDisjunction(ls)    => IMPSTheory.Or(ls map (x => doMathExp(x,thy)))
      case IMPSConjunction(ls)    => IMPSTheory.And(ls map (x => doMathExp(x,thy)))
      case q@IMPSLambda(_,_)      => doIMPSLambda(q, thy)
      case q@IMPSForAll(_,_)      => doIMPSForall(q, thy)
      case IMPSForSome(vs, r)     => IMPSTheory.Forsome(vs map (p => (LocalName(p._1.v), p._2 map (x => doSort(x,thy)))), doMathExp(r,thy))
      case IMPSImplication(p,q)   => IMPSTheory.Implies(doMathExp(p,thy), doMathExp(q,thy))
      case IMPSApply(f,ts)        => IMPSTheory.IMPSApply(doMathExp(f,thy), ts map (x => doMathExp(x,thy)))
      case IMPSIota(v1,s1,p)      => IMPSTheory.Iota(LocalName(v1.v), doSort(s1,thy), doMathExp(p,thy))
      case IMPSIotaP(v1,s1,p)     => IMPSTheory.IotaP(LocalName(v1.v), doSort(s1,thy), doMathExp(p,thy))
      case IMPSIsDefined(r)       => IMPSTheory.IsDefined(doMathExp(r,thy))
      case IMPSIsDefinedIn(r,s)   => IMPSTheory.IsDefinedIn(doMathExp(r,thy), doSort(s,thy))
      case IMPSUndefined(s)       => IMPSTheory.Undefined(doMathExp(s,thy))
    }
  }

  /* TRANSLATING SINGLE MATH EXPRESSIONS */
  /* These need to return lists of terms because there might be additional statements,
   * such as subtyping judgements. */

  /* This, in turn, represents a breach of convention.
   * This is excused by us not having to translate back to T, only to OMDOC. */

  def doIMPSLambda(lambda : IMPSLambda, thy : DeclaredTheory) : Term =
  {
    assert(lambda.vs.nonEmpty)
    val target : Term = doMathExp(lambda.t,thy)
    var foo = IMPSTheory.Lambda(lambda.vs map (p => (LocalName(p._1.v), p._2 map (x => doSort(x,thy)))), target)
    foo
  }

  def doIMPSForall(forall : IMPSForAll, thy : DeclaredTheory) : Term =
  {
    assert(forall.vs.nonEmpty)
    val target : Term = doMathExp(forall.p,thy)
    val foo = IMPSTheory.Forall(forall.vs map (p => (LocalName(p._1.v), p._2 map (x => doSort(x,thy)))), target)
    foo
  }
}
