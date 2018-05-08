package info.kwarc.mmt.imps

import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.utils.{JSONObject, JSONString}
import info.kwarc.mmt.imps.impsMathParser.IMPSMathParser

package object impsDefFormParsers
{
  def removeWhitespace(str : String) : String = {
    return str.replaceAll(" ", "").replaceAll("\t","").replaceAll("\n","").toLowerCase
  }

  /* Parser for IMPS special form def-atomic sort
   * Documentation: IMPS manual pgs. 158, 159 */
  def parseAtomicSort (e : Exp, js : List[JSONObject]) : Option[AtomicSort] =
  {
    // Required arguments
    var name : Option[String]         = None
    var qss  : Option[IMPSMathExp]    = None
    var thy  : Option[ArgumentTheory] = None

    // Optional arguments
    var usages  : Option[ArgumentUsages]  = None
    var witness : Option[Witness] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name = Some(x) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_)  => thy     = impsArgumentParsers.parseArgumentTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_)  => usages  = impsArgumentParsers.parseArgumentUsages(Exp(ds,src))
            case Exp(List(Str("witness")),_) => witness = impsArgumentParsers.parseWitness(Exp(ds,src))
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thy.get.thy)
      assert(json_theory.isDefined)
      assert(json_theory.get.getAsString("type") == "imps-theory")
      val defsorts : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"defsorts")
      assert(defsorts.nonEmpty)
      val thesort : Option[JSONObject] = defsorts.find(j => j.getAsString("name").toLowerCase == name.get.toLowerCase)
      assert(thesort.get.getAsString("type") == "imps-theory-defsort")
      assert(thesort.isDefined)

      val supersort : String = thesort.get.getAsString("sort")
      assert(supersort.endsWith(" prop)"))
      val finalsort : IMPSSort = IMPSAtomSort(supersort.takeWhile(c => c != ' ').tail)

      val thesexp : String = thesort.get.getAsString("formula-sexp")
      assert(thesexp.nonEmpty)

      val sp : SymbolicExpressionParser = new SymbolicExpressionParser
      val lsp = sp.parseAll(sp.parseSEXP,thesexp)
      assert(lsp.successful)

      qss = Some(impsMathParser.makeSEXPFormula(lsp.get))
      println("     > Formula generation successful: " + qss.get.toString)

      /* check for required arguments */
      if (name.isEmpty || qss.isEmpty || thy.isEmpty) None
      else { Some(AtomicSort(name.get, qss.get, thy.get, usages, witness, e.src, finalsort)) }

    } else { None }
  }

  /* Parser for IMPS special form def-constants
   * Documentation: IMPS manual pgs. 168, 169 */
  def parseConstant (e : Exp, js : List[JSONObject]) : Option[Constant] =
  {
    // Required arguments
    var name   : Option[String]         = None
    var defexp : Option[IMPSMathExp]    = None
    var thy    : Option[ArgumentTheory] = None

    // Optional arguments
    var usages : Option[ArgumentUsages] = None
    var sort   : Option[Sort]   = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

      var srtSR : Option[SourceRef] = None

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_)  => thy    = impsArgumentParsers.parseArgumentTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_)  => usages = impsArgumentParsers.parseArgumentUsages(Exp(ds,src))
            case Exp(List(Str("sort")),ssrc) => srtSR  = ssrc
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      println(" > looking for theory: " + thy.get.thy)
      val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thy.get.thy)
      assert(json_theory.isDefined)
      assert(json_theory.get.getAsString("type") == "imps-theory")
      val defconsts : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"defconsts")
      assert(defconsts.nonEmpty)
      val theconst : Option[JSONObject] = defconsts.find(j => j.getAsString("name").toLowerCase == name.get.toLowerCase)
      assert(theconst.get.getAsString("type") == "imps-theory-defconst")
      assert(theconst.isDefined)

      val thesexp : String = theconst.get.getAsString("formula-sexp")
      assert(thesexp.nonEmpty)

      val sp : SymbolicExpressionParser = new SymbolicExpressionParser
      val lsp = sp.parseAll(sp.parseSEXP,thesexp)
      assert(lsp.successful)

      defexp = Some(impsMathParser.makeSEXPFormula(lsp.get))
      println("     > Formula generation successful: " + defexp.get.toString)

      val thesort : String = theconst.get.getAsString("sort")
      assert(thesort != "null")

      val mp     = new IMPSMathParser()
      val parsed = mp.parseAll(mp.parseSort, thesort)
      assert(parsed.successful)
      sort = Some(Sort(parsed.get,srtSR))

      /* check for required arguments */
      if (name.isEmpty || defexp.isEmpty || thy.isEmpty) None
      else { println(" >> Success while trying to parse constant " + name.get + " : " + sort.get.sort.toString) ; Some(Constant(name.get, defexp.get, thy.get, sort.get.sort, usages, e.src)) }
    } else { println(" >> Failure while trying to parse constant " + e.children(1)) ; None }
  }

  /* Parser for IMPS special form def-constants
   * Documentation: IMPS manual pgs. 168, 169 */
  def parseRecursiveConstant (e : Exp, js : List[JSONObject]) : Option[RecursiveConstant] =
  {
    // Required arguments
    var names   : List[String]      = Nil
    var defexps : List[IMPSMathExp] = Nil
    var sorts   : List[IMPSSort]    = Nil

    var thy     : ArgumentTheory    = null

    // Optional arguments
    var usages   : Option[ArgumentUsages] = None
    var definame : Option[DefinitionName] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    assert(cs >= 3)

    /* Parse positional arguments */
    e.children(1) match
    {
      case Exp(List(Str(x)), _)   => names = List(x)
      case Exp(List(Exp(cs,_)),_) => {
        for (c <- cs) {
          c match {
            case Exp(List(Str(x)), _) => names = names ::: List()
            case _ => ???
          }
        }
      }
      case Exp(List(Exp(List(Str(x)),_), Exp(List(Str(y)),_)),_) => names = List(x,y)
      case _ => println(e.children(1)) ; assert(false)
    }

    println(" > recusive constant: parsed " + names.length + " name(s): " + names.toString())

    /* Parse keyword arguments, these can come in any order */
    var i : Int = 1
    while (cs - i > 0)
    {
      e.children(i) match {
        case Exp(ds,src) => ds.head match
        {
          case Exp(List(Str("theory")),_)           => thy      = impsArgumentParsers.parseArgumentTheory(Exp(ds,src)).get
          case Exp(List(Str("usages")),_)           => usages   = impsArgumentParsers.parseArgumentUsages(Exp(ds,src))
          case Exp(List(Str("definition-name")),_)  => definame = impsArgumentParsers.parseDefinitionName(Exp(ds,src))
          case _                                    => ()
        }
        case _ => ()
      }
      i += 1
    }

    assert(names.nonEmpty)
    val sp : SymbolicExpressionParser = new SymbolicExpressionParser
    val mp : IMPSMathParser           = new IMPSMathParser()

    val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thy.thy)
    assert(json_theory.isDefined)
    assert(json_theory.get.getAsString("type") == "imps-theory")

    val recconsts : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"def-recursive-consts")
    assert(recconsts.nonEmpty)

    val theconst : Option[JSONObject] = recconsts.find(j => j.getAsList(classOf[JSONString], "names").map(_.value.toLowerCase).contains(names(0).toLowerCase))
    assert(theconst.isDefined)
    assert(theconst.get.getAsString("type") == "imps-theory-recursive-constant")

    val thesexps = theconst.get.getAsList(classOf[JSONString],"defining-sexps")
    assert(thesexps.nonEmpty)
    var realsexps : List[String] = thesexps.map(_.value)
    if (thesexps.lengthCompare(names.length) != 0)
    {
      val nusexps = thesexps.head.value.split(", ")
      assert(nusexps.lengthCompare(names.length) == 0)
      realsexps = nusexps.toList
    }

    for (k <- names.indices)
    {
      println(" > Working on recursive constant " + names(k))

      val thesexp = realsexps(k)
      val lsp = sp.parseAll(sp.parseSEXP,thesexp)
      assert(lsp.successful)

      val defexp = impsMathParser.makeSEXPFormula(lsp.get)
      defexps = defexps ::: List(defexp)
      println("     > Formula generation successful: " + defexp.toString)

      var thesorts  : List[String]     = theconst.get.getAsList(classOf[JSONString],"sortings").map(g => g.value)

      if (thesorts.length < names.length)
      {
        assert(thesorts.length == 1)
        thesorts = thesorts.head.split(", ").toList
      }

      assert(thesorts.nonEmpty)
      assert(thesorts.lengthCompare(names.length) == 0)

      val parsed = mp.parseAll(mp.parseSort, thesorts(k))
      assert(parsed.successful)
      sorts = sorts ::: List(parsed.get)
    }

    assert(names.nonEmpty)
    assert(defexps.nonEmpty)
    assert(sorts.nonEmpty)
    assert(names.length == defexps.length)

    Some(RecursiveConstant(names, defexps, sorts, thy, usages, definame, e.src))
  }

  /* Parser for IMPS special form def-quasi-constructor
   * Documentation: IMPS manual pgs. 177, 178 */
  def parseQuasiConstructor (e : Exp) : Option[QuasiConstructor] =
  {
    // Required arguments
    var name   : Option[String]           = None
    var expstr : Option[String]           = None
    var lang   : Option[ArgumentLanguage] = None

    // Optional arguments
    var fixed  : Option[FixedTheories] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => expstr = Some(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("language")),_)       => lang  = impsArgumentParsers.parseArgumentLanguage(Exp(ds,src)) ; assert(lang.isDefined)
            case Exp(List(Str("fixed-theories")),_) => fixed = impsArgumentParsers.parseFixedTheories(Exp(ds,src))    ; assert(fixed.isDefined)
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || expstr.isEmpty || lang.isEmpty) None
      else { Some(QuasiConstructor(name.get, expstr.get, lang.get, fixed, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-imported-rewrite-rules
   * Documentation: IMPS manual pg. 169 */
  def parseImportedRewriteRules (e : Exp) : Option[ImportedRewriteRules] =
  {
    // Required arguments
    var name   : Option[String]   = None

    // Optional arguments
    var srcTheory   : Option[SourceTheory]   = None
    var srcTheories : Option[SourceTheories] = None

    val cs : Int = e.children.length

    if (cs >= 2)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 2
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("src-theory")),_)   => srcTheory   = impsArgumentParsers.parseSourceTheory(Exp(ds,src))
            case Exp(List(Str("src-theories")),_) => srcTheories = impsArgumentParsers.parseSourceTheories(Exp(ds,src))
            case _                                => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || (srcTheory.isEmpty && srcTheories.isEmpty)) None
      else { Some(ImportedRewriteRules(name.get, srcTheory, srcTheories, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-schematic-macete
   * Documentation: IMPS manual pgs. 180, 181 */
  def parseSchematicMacete (e : Exp) : Option[SchematicMacete] =
  {
    // Required arguments
    var name    : Option[String] = None
    var formula : Option[String] = None
    var thy     : Option[ArgumentTheory] = None

    // Optional arguments
    var nullarg  : Boolean = false
    var transarg : Boolean = false

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name    = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => formula = Some(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_) => thy = impsArgumentParsers.parseArgumentTheory(Exp(ds,src))
            case (Str("null"))              => nullarg  = true
            case (Str("transportable"))     => transarg = true
            case _                          => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || formula.isEmpty || thy.isEmpty) None
      else { Some(SchematicMacete(name.get, formula.get, thy.get, nullarg, transarg, e.src)) }

    } else { None }
  }

  def parseTranslation (e : Exp, js : List[JSONObject]) : Translation =
  {
    /* Required arguments */
    var name    : Option[String] = None
    var source  : Option[TranslationSource] = None
    var target  : Option[TranslationTarget] = None

    /* Optional arguments */
    var force      : Boolean = false
    var forceQL    : Boolean = false
    var dontEnrich : Boolean = false

    var fixed       : Option[FixedTheories]             = None
    var assumptions : Option[Assumptions]               = None
    var sortpairs   : Option[SortPairs]                 = None
    var constpairs  : Option[ConstantPairs]             = None
    var coretrans   : Option[CoreTranslation]           = None
    var theintcheck : Option[TheoryInterpretationCheck] = None

    /* Parse positional arguments */
    e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

    /* Parse modifier and keyword arguments, these can come in any order */
    val csl : Int = e.children.length
    var i : Int = 2
    while (csl - i > 0)
    {
      e.children(i) match
      {
        case Exp(ds,src) => ds.head match
        {
          case Exp(List(Str("source")),_) => {
            source = impsArgumentParsers.parseSource(Exp(ds,src))
            assert(source.isDefined)
          }
          case Exp(List(Str("target")),_) => {
            target = impsArgumentParsers.parseTarget(Exp(ds,src))
            assert(target.isDefined)
          }
          case Exp(List(Str("assumptions")),_) => {
            assumptions = impsArgumentParsers.parseAssumptions(Exp(ds,src),js)
            assert(assumptions.isDefined)
          }
          case Exp(List(Str("fixed-theories")),_) => {
            fixed = impsArgumentParsers.parseFixedTheories(Exp(ds,src))
            assert(fixed.isDefined)
          }
          case Exp(List(Str("sort-pairs")),_) => {
            sortpairs = impsArgumentParsers.parseSortPairs(Exp(ds,src),js)
            assert(sortpairs.isDefined)
          }
          case Exp(List(Str("constant-pairs")),_) => {
            constpairs = impsArgumentParsers.parseConstantPairs(Exp(ds,src),js)
            assert(constpairs.isDefined)
          }
          case Exp(List(Str("core-translation")),_) => {
            coretrans = impsArgumentParsers.parseCoreTranslation(Exp(ds,src))
            assert(coretrans.isDefined)
          }
          case Exp(List(Str("theory-interpretation-check")),_) => {
            theintcheck = impsArgumentParsers.parseTheoryInterpretationCheck(Exp(ds,src))
            assert(theintcheck.isDefined)
          }
          case (Str("force"))                  => force      = true
          case (Str("force-under-quick-load")) => forceQL    = true
          case (Str("dont-enrich"))            => dontEnrich = true
          case zzz                             => println(" ~~~ argument to translation not parsed: " + zzz.toString); ()
        }
        case _ => ()
      }
      i += 1
    }
    assert(name.nonEmpty)
    Translation(name.get,force,forceQL,dontEnrich,source.get,target.get,fixed,assumptions,sortpairs,constpairs,coretrans,theintcheck,e.src)
  }

  /* Parser for IMPS form def-theorem
   * Documentation: IMPS manual pgs. 184 ff. */
  def parseTheorem (e : Exp, js : List[JSONObject]) : Option[Theorem] =
  {
    /* Required arguments */
    var name    : Option[String]         = None
    var formula : Option[IMPSMathExp]    = None
    var theory  : Option[ArgumentTheory] = None

    /* Optional Arguments */
    var lemma   : Boolean = false
    var reverse : Boolean = false

    var usages : Option[ArgumentUsages]      = None
    var hmthy  : Option[HomeTheory]          = None
    var trans  : Option[ArgumentTranslation] = None
    var macete : Option[Macete]              = None
    var prf    : Option[Proof]               = None

    /* Three arguments minimum */
    val csl : Int = e.children.length
    assert(csl >= 3)

    /* Parse positional arguments, these must be in this order */
    e.children(1) match
    {
      case Exp(List(Str(x)), _) => name = Some(x)
      case Exp(List(),_)        => name = Some("()") // Theorems can have this name, according to documentation.
                                                     // Turns out parsers get easily confused here.
    }
    assert(name.isDefined)

    /* Formula will be parsed later */

    /* Parse modifier and keyword arguments, these can come in any order */
    var i : Int = 3
    while (csl - i > 0)
    {
      e.children(i) match
      {
        case Exp(ds,src) => ds.head match
        {
          case Exp(List(Str("theory")),_)      => { theory  = impsArgumentParsers.parseArgumentTheory(Exp(ds,src)) ; assert(theory.isDefined) }
          case Exp(List(Str("home-theory")),_) => { hmthy   = impsArgumentParsers.parseHomeTheory(Exp(ds,src))     ; assert(hmthy.isDefined)  }
          case Exp(List(Str("usages")),_)      => { usages  = impsArgumentParsers.parseArgumentUsages(Exp(ds,src)) ; assert(usages.isDefined) }
          case Exp(List(Str("macete")),_)      => { macete  = impsArgumentParsers.parseMacete(Exp(ds,src))         ; assert(macete.isDefined) }
          case Exp(List(Str("translation")),_) => { trans   = impsArgumentParsers.parseTranslation(Exp(ds,src))    ; assert(trans.isDefined)  }
          case Exp(List(Str("proof")),_)       => prf     = impsArgumentParsers.parseProofScript(Exp(ds,src))
          case (Str("lemma"))                  => reverse = true
          case (Str("reverse"))                => lemma   = true
          case zzz                             => println(" ~~~ argument to theorem not parsed: " + zzz.toString); ()
        }
        case _ => ()
      }
      i += 1
    }
    assert(name.nonEmpty)
    assert(theory.nonEmpty)

    println(" > Looking for theory " + theory.get.thy)
    val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == theory.get.thy.toLowerCase)
    assert(json_theory.isDefined)
    assert(json_theory.get.getAsString("type") == "imps-theory")
    val theorems : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"theorems")
    assert(theorems.nonEmpty)
    val axioms : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"axioms")
    var s : String = ""
    val thetheorem : Option[JSONObject] = if (name.get != "()")
    {
      println(" > looking for theorem " + name.get + " in theory " + theory.get.thy + " ...")
      (axioms ::: theorems).find(j => j.getAsString("name") == name.get.toLowerCase)
    } else {
      println(" > looking for nameless theorem () in theory " + theory.get.thy + " ...")
      e.children(2) match {
        case Exp(List(Str(x)), _) => {
          s = x.tail.init
          println("     > scala theorem: " + removeWhitespace(x.tail.init))
          var tempt = theorems.find(j => removeWhitespace(j.getAsString("formula-string")) == removeWhitespace(x.tail.init))

          if (!(tempt.isDefined)) {
            val handpicked = handpick(removeWhitespace(x.tail.init))
            println("     > handpicked: " +handpicked)
            tempt = theorems.find(j => removeWhitespace(j.getAsString("formula-string")) == handpicked)
          }

          tempt
        }
        case _                    => ???
      }
    }

    if (!(thetheorem.isDefined))
    {
      assert(s != "")
      val bar = removeWhitespace(s)
      println(" > s = " + s)

      for (t <- theorems)
      {
        val foo = removeWhitespace(t.getAsString("formula-string"))
        val n = 5
        if (foo.take(n) == bar.take(n)) {
          println("     > json theorem: " + foo)
        }
      }
    }

    assert(thetheorem.isDefined)
    val thesexp : String = thetheorem.get.getAsString("formula-sexp")
    assert(thesexp.nonEmpty)

    val sp : SymbolicExpressionParser = new SymbolicExpressionParser
    val lsp = sp.parseAll(sp.parseSEXP,thesexp)
    assert(lsp.successful)

    formula = Some(impsMathParser.makeSEXPFormula(lsp.get))
    println("     > Formula generation successful: " + formula.get.toString)

    assert (!(name.isEmpty || formula.isEmpty || theory.isEmpty))
    Some(Theorem(name.get, formula.get, lemma, reverse, theory.get, usages, trans, macete, hmthy, prf, e.src))
  }

  /* Parser for IMPS special form def-language
   * Documentation: IMPS manual pgs. 172 - 174 */
  def parseLanguage (e : Exp) : Option[Language] =
  {
    /* Required Arguments */
    var name : Option[String] = None

    /* Optional Arguments */
    var embedlang  : Option[EmbeddedLanguage]       = None
    var embedlangs : Option[EmbeddedLanguages]      = None
    var bstps      : Option[LangBaseTypes]          = None
    var extens     : Option[Extensible]             = None
    var srts       : Option[SortSpecifications]     = None
    var cnstnts    : Option[ConstantSpecifications] = None

    if (e.children.nonEmpty)
    {
      /* Parse positional arguments, these must be in this order */
      e.children(1) match {
        case Exp(List(Str(x)), _) => name = Some(x)
      }

      /* Parse keyword arguments. These can be in any order */
      for (c <- e.children.tail)
      {
        c match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("embedded-languages")),_) => embedlangs = impsArgumentParsers.parseEmbeddedLangs(Exp(ds,src))
            case Exp(List(Str("embedded-language")),_)  => embedlang  = impsArgumentParsers.parseEmbeddedLang(Exp(ds,src))
            case Exp(List(Str("base-types")),_)         => bstps      = impsArgumentParsers.parseLanguageBaseTypes(Exp(ds,src))
            case Exp(List(Str("sorts")),_)              => srts       = impsArgumentParsers.parseSortsArgument(Exp(ds,src))
            case Exp(List(Str("extensible")),_)         => extens     = impsArgumentParsers.parseExtensible(Exp(ds,src))
            case Exp(List(Str("constants")),_)          => cnstnts    = impsArgumentParsers.parseConstants(Exp(ds,src))
            case _                                      => ()
          }
          case _ => ()
        }

      }

      if (name.isEmpty) { None }
      else { Some(Language(name.get, embedlang, embedlangs, bstps, extens, srts, cnstnts, e.src)) }
    } else { None }
  }

  /* Parser for IMPS special form def-language
   * Documentation: IMPS manual pgs. 186-188 */
  def parseTheory(e : Exp, js : List[JSONObject]) : Option[Theory] =
  {
    /* Required Arguments */
    var name : Option[String] = None

    /* Optional Arguments */
    var lang      : Option[ArgumentLanguage]  = None
    var cmpntthrs : Option[ComponentTheories] = None
    var axioms    : Option[TheoryAxioms]      = None
    var dstnct    : Option[DistinctConstants] = None

    if (e.children.nonEmpty)
    {
      /* Parse positional arguments, these must be in this order */
      e.children(1) match {
        case Exp(List(Str(x)), _) => name = Some(x)
      }
      assert(name.isDefined)

      /* Parse keyword arguments. These can be in any order */
      for (c <- e.children.tail)
      {
        c match {
          case Exp(ds, src) => ds.head match {
            case Exp(List(Str("language")), _) => lang = impsArgumentParsers.parseArgumentLanguage(Exp(ds, src))
            case Exp(List(Str("component-theories")), _) => cmpntthrs = impsArgumentParsers.parseComponentTheories(Exp(ds, src))
            case Exp(List(Str("distinct-constants")), _) => dstnct = impsArgumentParsers.parseDistinctConstants(Exp(ds, src))
            case Exp(List(Str("axioms")), _) => axioms = impsArgumentParsers.parseTheoryAxioms(Exp(ds,src), js, name.get)
            case _ => ()
          }
          case _ => ()
        }
      }

      if (name.isEmpty) { None }
      else { Some(Theory(name.get, lang, cmpntthrs, axioms, dstnct, e.src)) }
    } else { None }
  }

  /* Welcome to the most shameful part of the codebase...
   * Some strings are subtly different from JSON to T, so we correct by hand.
   */
  def handpick(str: String) : String =
  {
    str match {
      case "forall(x:rr,#(abs(x)))" => "total_q{abs,[rr,rr]}"
      case "forall(x,y:rr,#(max(x,y)))" => "total_q{max,[rr,rr,rr]}"
      case "forall(x,y:pp,#(dist(x,y)))" => "total_q{dist,[pp,pp,rr]}"
      case "total_q(+_kk,[kk,kk,kk])" => "total_q{+_kk,[kk,kk,kk]}"
      case "total_q(*_kk,[kk,kk,kk])" => "total_q{*_kk,[kk,kk,kk]}"
      case "total_q(-_kk,[kk,kk])" => "total_q{-_kk,[kk,kk]}"
      case "total_q(**,[uu,uu,uu])" => "total_q{**,[uu,uu,uu]}"
      case "forall(x:rr,x+(-x)=0)" => "forall(x:rr,x+-x=0)"
      case "forall(y,x:rr,x-y=x+(-y))" => "forall(y,x:rr,x-y=x+-y)"
      case "forall(n,m:zz,x:rr,((#(x^m,rr)and#(x^n,rr))iff#((x^m)^n,rr)))" => "forall(n,m:zz,x:rr,(#(x^m,rr)and#(x^n,rr))iff#((x^m)^n,rr))"
      case "total_q(+,[rr,rr,rr])" => "total_q{+,[rr,rr,rr]}"
      case "total_q(*,[rr,rr,rr])" => "total_q{*,[rr,rr,rr]}"
      case "total_q(-,[rr,rr])" => "total_q{-,[rr,rr]}"
      case "total_q(sub,[rr,rr,rr])" => "total_q{sub,[rr,rr,rr]}"
      case "forall(x,y,z:kk,not(x=o_kk)andnot(y=o_kk)andnot(z=o_kk)impliesnot(x*_kky=o_kk)and(not(y*_kkz=o_kk)andx*_kky*_kkz=x*_kk(y*_kkz)))" => "forall(x,y,z:kk,not(x=o_kk)andnot(y=o_kk)andnot(z=o_kk)impliesnot(x*y=o_kk)and(not(y*z=o_kk)andx*y*z=x*(y*z)))"
      case "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andinv(x)*_kkx=i_kk)" => "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andinv(x)*x=i_kk)"
      case "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andx*_kkinv(x)=i_kk)" => "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andx*inv(x)=i_kk)"
      case "forall(n,m:zz,x:kk,((#(x^m,kk)and#(x^n,kk))iff#((x^m)^n,kk)))" => "forall(n,m:zz,x:kk,(#(x^m,kk)and#(x^n,kk))iff#((x^m)^n,kk))"
      case "total_q{iterate,[[pp,pp],pp,[zz,pp]]}" => "total_q{ms%iterate,[[pp,pp],pp,[zz,pp]]}"
      case "forsome(a:sets[uu],equiv%class_q(a))" => "nonvacuous_q{equiv%class_q}"
      case _ => str
    }

  }
}