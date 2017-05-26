package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import archives._
import documents._
import modules._
import notations._
import symbols._

object GenericScalaExporter {
  /** reserved identifiers */
  private val keywords = List("true", "false", "type", "val", "var", "def", "abstract", "implicit", "class", "trait", "object", "extends", "with", "while", "do", "for")
  /** preused identifiers, i.e., declared in Object */
  private val reserved = List("eq", "List", "Set", "String")

  /** escapes strings to avoid clashes with Scala keywords */
  private def escape(s: String): String = {
    if (keywords.contains(s))
      "`" + s + "`"
    else if (reserved.contains(s))
      "N" + s
    else escapeChars(s)
  }

  // TODO: do this cleanly
  private def escapeChars(s: String) =
    s.replace("_","_underscore_").replace("≃","_cong_").replace("-","_minus_")
  private def unescapeChars(s: String) = 
    s.replace("_cong_","≃").replace("_minus_","-").replace("_underscore_", "_")
    
    
  def nameToScalaQ(p: GlobalName) = (p.module.name.toPath + "_" + escapeChars(p.name.toPath)).replace(".", "__")

  def nameInScala(p: GlobalName) = (p.module.name.toPath).replace(".", "__") + "." + escapeChars(p.name.toPath) + ".path"

  def nameToScala(l: LocalName) = escape(l.toPath.replace("/", "."))

  /** package URI */
  def dpathToScala(d: DPath, key: List[String] = Nil) = {
    val u = d.uri
    var auth = utils.stringToList(u.authority.getOrElse(""), "\\.").reverse
    (auth ::: key ::: u.path).map(escapeChars).mkString(".")
  }

  def scalaToDPath(j: String, key: List[String] = Nil) = {
    val segments = j.split("\\.").toList.map(unescapeChars)
    val i = segments.indexOfSlice(key)
    val (auth, path) = if (key == Nil || i == -1)
      (segments.reverse, Nil)
    else
      (segments.take(i).reverse, segments.drop(i + key.length))
    DPath(utils.URI("http", auth.mkString(".")) / path)
  }

  /** package URI . modname */
  def mpathToScala(m: MPath, key: List[String] = Nil) = dpathToScala(m.doc, key) + "." + nameToScala(m.name)

  val imports = "import info.kwarc.mmt.api._\n" + "import objects._\n" + "import uom._\n" +
    "import ConstantScala._\n"

  def scalaVal(name: String, tp: String): String = "  val " + name + " : " + tp

  def scalaVal(name: GlobalName, tp: String): String = scalaVal(nameToScalaQ(name), tp)

  def scalaValDef(name: String, tp: Option[String], df: String): String = {
    val tpS = tp.map(": " + _).getOrElse("")
    "  lazy val " + name + tpS + " = " + df
  }

  def scalaValDef(name: GlobalName, tp: Option[String], df: String): String = scalaValDef(nameToScalaQ(name), tp, df)

  def scalaDef(name: String, args: List[(String, String)], ret: String): String = {
    val argsB = if (args.isEmpty) "" else args.map({ case (x, y) => s"$x: $y" }).mkString("(", ", ", ")")
    "  def " + name + argsB + ": " + ret
  }

  def scalaDef(name: GlobalName, args: List[(String, String)], ret: String): String = scalaDef(nameToScalaQ(name), args, ret)

  def scalaType(name: GlobalName): String = "  type " + nameToScalaQ(name)

}

import GenericScalaExporter._

/** This trait bundles auxiliary methods for exporting Scala code */
class GenericScalaExporter extends Exporter {
  val outDim = Dim("export", "scala")
  val key = "scala"
  override val outExt = "scala"
  override protected val folderName = "NAMESPACE"
  val packageSep: List[String] = Nil

  /* top level export methods */
  
  def exportTheory(t: DeclaredTheory, bf: BuildTask) {
    outputHeader(t.parent.doc)
    outputTrait(t)
    outputCompanionObject(t) { c =>
      c.not.map(n => applyMethods(n.arity)).getOrElse("")
    }
  }

  def exportView(v: DeclaredView, bf: BuildTask) {}

  /** produces code to instantiate [[uom.DocumentScala]] to iterate over all content */
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
    var pack = dpathToScala(dpath, packageSep)
    if (pack == "") pack = "content" // dpath is empty URI for the content folder
    rh.writeln("//Source file generated by MMT\n")
    rh.writeln(s"package $pack")
    rh.writeln("import info.kwarc.mmt.api.uom._\n\n")
    rh.writeln(s"object $folderName extends DocumentScala {")
    modules foreach {bt =>
      val m = controller.globalLookup.getModule(bt.contentMPath)
      rh.writeln(outputModuleEntry(m))
    }
    namespaces foreach { case bt =>
      val p = dpathToScala(bt.contentDPath, packageSep)
      rh.writeln(s"  addDocument($p.$folderName)")
    }
    rh.writeln("}")
  }

  /** command for a module to be added to the namespace object, override as needed */
  def outputModuleEntry(m: Module) : String = ""
  
  /** do nothing by default */
  def exportDocument(doc: Document, bt: BuildTask) {}

  /* theories are exported as 3 parts: header, optional trait, object */
  
  protected def outputHeader(dp: DPath) {
    val pack = dpathToScala(dp, packageSep)
    rh.writeln("package " + pack)
    rh.writeln(imports)
  }

  /**
   * generates the trait, empty by default, override as needed
   */
  protected def outputTrait(t: DeclaredTheory) {}

  /* the companion object */
  
  /** generates a companion object with fields for the MMT URIs
    * @param extraFields fields appended to the object
    */
  protected def outputCompanionObject(t: DeclaredTheory)(extraFields: Constant => String) {
    val tpathS = t.path.toString
    val name = nameToScala(t.name)
    rh.writeln(s"/** Convenience functions for the MMT URIs of the declarations in the theory $tpathS\n" +
      "    along with apply/unapply methods for them */")
    rh.writeln(s"object $name extends TheoryScala {")
    val baseUri = t.parent.uri
    rh.writeln("  val _base = DPath(utils.URI(\"" + baseUri.scheme.getOrElse("") +
      "\", \"" + baseUri.authority.getOrElse("") + "\")" +
      baseUri.path.foldRight("")((a, b) => " / \"" + a + "\"" + b) +
      ")"
    )
    rh.writeln("  val _name = LocalName(\"" + t.name + "\")")
    t.getPrimitiveDeclarations foreach {
      case c: Constant =>
        var o = ""
        o += s"\n  object ${nameToScala(c.name)} extends ConstantScala {\n"
        o += s"    val parent = _path\n"
        o += "    val name = \"" + c.name + "\"\n"
        o += extraFields(c)
        o += "  }"
        rh.writeln(o)
      case _ =>
    }
    rh.writeln("\n}")
  }

  private def arityToScala(arity: Arity): List[(String, String)] = arity.components.map {
    case SimpArg(n, _) => ("x" + n.abs, "Term")
    case LabelArg(n,_,_) => ("x" + n.abs, "OML")
    case ImplicitArg(n, _) => ("x" + n.abs, "Term")
    case SimpSeqArg(n, _, _) => ("xs" + n.abs, "List[Term]")
    case LabelSeqArg(n, _, _,_) => ("xs" + n.abs, "List[OML]")
    case Var(n, _, None, _) => ("v" + n, "VarDecl")
    case Var(n, _, Some(_), _) => ("vs" + n, "Context")
  }

  private def lastArgIsSeq(arity: Arity) = arity.arguments.nonEmpty && arity.arguments.last.isSequence

  private def lastVarIsSeq(arity: Arity) = arity.variables.nonEmpty && arity.variables.last.isSequence

  private def applyMethods(arity: Arity): String = {
    val scalaArgs = arityToScala(arity)
    // x1 :: ... :: xn :: Nil or x1 :: ... :: xsn
    var argListString = scalaArgs.map(_._1).mkString(" :: ")
    if (!lastArgIsSeq(arity))
      argListString = argListString + ":: scala.Nil"
    // (x1, ..., xn) or x1
    var argTupleString = scalaArgs.map(_._1).mkString(", ")
    if (scalaArgs.length > 1)
      argTupleString = s"($argTupleString)"
    // (T1, ..., Tn) or T1
    var tpString = scalaArgs.map(_._2).mkString(", ")
    if (scalaArgs.length > 1)
      tpString = s"($tpString)"
    // x1: T1, ..., xn: Tn
    val argtpString = scalaArgs.map(p => p._1 + ": " + p._2).mkString(", ")
    // def apply(...) : Term = ...
    val app = if (arity.isApplication)
      s"    def apply($argtpString) = OMA(OMID(this.path), $argListString)\n"
    else if (arity.isPlainBinder)
      s"    def apply(vs1: Context, s2: Term) = OMBIND(OMID(this.path), vs1, s2)\n"
    else if (arity.isConstant)
      ""
    else
      "  // no apply method generated for this arity\n"
    // def unapply(t: Term): Option[...] = ...
    val unapp = if (arity.isApplication)
      s"    def unapply(t: Term): Option[$tpString] = t match {\n" +
        s"      case OMA(OMID(this.path), $argListString) => Some($argTupleString)\n" +
        s"      case _ => None\n" +
        s"    }\n"
    else if (arity.isPlainBinder)
      s"    def unapply(t: Term): Option[(Context, Term)] = t match {\n" +
        s"      case OMBIND(OMID(this.path), vs1, s2) => Some((vs1, s2))\n" +
        s"      case _ => None\n" +
        s"    }\n"
    else if (arity.isConstant)
      ""
    else
      "  // no unapply methods generated for this arity\n"

    app + unapp
  }
}
