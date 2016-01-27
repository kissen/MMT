package info.kwarc.mmt.api.frontend

import java.nio.file.{Files, Paths}

import scala.util.Try

/** describe the kind of a command line option */
sealed abstract class OptionArgument

/** just a flag */
case object NoArg extends OptionArgument

/** expect an integer argument */
case object IntArg extends OptionArgument

/** expect an optional integer argument that must not be given as a separate argument */
case object OptIntArg extends OptionArgument

case object StringArg extends OptionArgument

/** collect multiple values */
case object StringListArg extends OptionArgument

/** the possible values given by options */
sealed abstract class OptionValue {
  /** convenience methods */
  def getStringList: List[String] = this.asInstanceOf[StringListVal].value

  def getStringVal: String = this.asInstanceOf[StringVal].value

  def getIntVal: Int = this.asInstanceOf[IntVal].value
}

case object NoVal extends OptionValue

case class IntVal(value: Int) extends OptionValue

case class StringVal(value: String) extends OptionValue

case class StringListVal(value: List[String]) extends OptionValue

/** a description of one option */
case class OptionDescr(long: String, short: String, arg: OptionArgument, description: String) {
  def shortMatch(opt: String) = short.nonEmpty && "-" + short == opt

  def longExact(opt: String) = "--" + long == opt

  def longEqual(opt: String) = opt.startsWith("--" + long + "=")

  def exactMatch(opt: String) = shortMatch(opt) || longExact(opt)

  def mayMatch(opt: String) = exactMatch(opt) || longEqual(opt)
}

/** helper function for parsing command line arguments */
object AnaArgs {
  type OptionDescrs = List[OptionDescr]
  type OptionMap = Map[String, OptionValue]

  /** extract matching options */
  def apply(opts: OptionDescrs, args: List[String]): (OptionMap, List[String]) = {
    args match {
      case Nil => (Map.empty, Nil)
      case hd :: tl =>
        val optO = opts.find(_.mayMatch(hd))
        optO match {
          case None =>
            val (m, r) = apply(opts, tl)
            (m, hd :: r)
          case Some(o) =>
            val equalOpt = o.longEqual(hd)
            val checkTail = equalOpt || o.arg == NoArg || tl.isEmpty ||
              o.arg == OptIntArg && toOptInt(tl.head).isEmpty
            val v = if (equalOpt)
               Some(hd.substring(o.long.length + 3))
            else if (checkTail)
               None
            else
               Some(tl.head)
            val (m, r) = apply(opts, if (checkTail) tl else tl.tail)
            getOptOptionValue(m, o, v) match {
              case None => (m, hd :: (if (checkTail) r else tl.head :: r))
              case Some(e) => (m + (o.long -> e), r)
            }
        }
    }
  }

  def getStringList(m: OptionMap, arg: String): List[String] =
    m.getOrElse(arg, StringListVal(Nil)).getStringList

  private def toOptInt(s: String) = Try(s.toInt).toOption

  private def getOptOptionValue(m: OptionMap, o: OptionDescr, value: Option[String]): Option[OptionValue] = {
    if (m.get(o.long).isDefined && o.arg != StringListArg) None // already defined
    else {
      value match {
        case None =>
          if (o.arg == NoArg || o.arg == OptIntArg) Some(NoVal) else None
        case Some(v) =>
          o.arg match {
            case OptIntArg | IntArg =>
              toOptInt(v).map(IntVal)
            case StringArg =>
              Some(StringVal(v))
            case StringListArg =>
              Some(StringListVal(v ::
                m.getOrElse(o.long, StringListVal(Nil)).getStringList))
            case _ =>
              None
          }
      }
    }
  }

  def splitOptions(args: List[String]): (List[String], List[String]) =
    args.partition(_.startsWith("-"))

  def usageMessage(optionDescrs: OptionDescrs): List[String] =
    "supported command line options:" ::
    optionDescrs.map { case OptionDescr(long, short, optArg, descr) =>
      val longOpt =
        (if (optArg == StringListArg) " (--" else "  --") +
          long + (optArg match {
        case NoArg => " "
        case IntArg => "=INT "
        case OptIntArg => "[=N] "
        case StringArg => "=STR "
        case StringListArg => "=STR)+ "
      })
      (if (short.nonEmpty) {
        "  -" + short +
          (optArg match {
          case NoArg => "    "
          case IntArg => " INT"
          case OptIntArg => " [N]"
          case _ => " STR"
        })
      }
      else "        ") + longOpt +
        List.fill(24)(" ").mkString.substring(Math.min(longOpt.length, 23)) +
        descr
    }
}

/**
  * Represents arguments passed to MMT on the command line.
  *
  * @param help       Should we print a help text
  * @param about      Should we print an about text
  * @param send       Should we send commands to a remote port instead of running them locally?
  * @param mmtFiles   List of MMT files to load
  * @param scalaFiles List of Scala files to load
  * @param cfgFiles   List of config files to load
  * @param commands   List of commands to run
  * @param shell      interactive shell requested
  * @param noshell    interactive shell unrequested
  * @param keepalive  keepalive requested
  */
case class ShellArguments(
   help: Boolean,
   about: Boolean,
   send: Option[Int],
   mmtFiles: List[String],
   scalaFiles: List[String],
   cfgFiles: List[String],
   commands: List[String],
   shell: Boolean,
   noshell: Boolean,
   keepalive: Boolean,
   useQueue: Boolean
 ) {
   /** decides whether or not to show the shell
    *  default behavior: show shell if nothing else is happening
    */
   def prompt = if (shell) true else if (noshell) false else {
      send.isEmpty && commands.isEmpty
   }
   /** do not keep alive but terminate/cleanup processes and exit after commands have been processed */
   def runCleanup = ! keepalive && ! prompt
}

import AnaArgs._

object ShellArguments {
  val toplevelArgs: OptionDescrs = List(
    OptionDescr("help", "h", NoArg, "command line help"),
    OptionDescr("about", "a", NoArg, "about the program"),
    OptionDescr("shell", "i", NoArg, "start an interactive shell"),
    OptionDescr("keepalive", "w", NoArg, "wait for processes to finish"),
    OptionDescr("noshell", "w", NoArg, "same as keepalive"),
    OptionDescr("send", "", IntArg, "send commands to remote port"),
    OptionDescr("mbt", "", StringListArg, "mbt input file "),
    OptionDescr("file", "", StringListArg, "msl input file"),
    OptionDescr("cfg", "", StringListArg, "config input file"),
    OptionDescr("queue", "", NoArg, "start a build queue")
  )

  def parse(arguments: List[String]): Option[ShellArguments] = {
    val (m, cs) = AnaArgs(toplevelArgs, arguments)
    val helpFlag = m.get("help").isDefined
    val aboutFlag = m.get("about").isDefined
    val os = m.get("keepalive").toList ++ m.get("shell").toList ++ m.get("noshell").toList
    val sa = ShellArguments(
      help = helpFlag,
      about = aboutFlag,
      send = m.get("send").map(a => a.asInstanceOf[IntVal].value),
      mmtFiles = getStringList(m, "file"),
      scalaFiles = getStringList(m, "mbt"),
      cfgFiles = getStringList(m, "cfg"),
      commands = cs,
      shell = m.get("shell").isDefined,
      noshell = m.get("noshell").isDefined,
      keepalive = m.get("keepalive").isDefined,
      useQueue = m.get("queue").isDefined
    )
    val fs = sa.mmtFiles ++ sa.scalaFiles ++ sa.cfgFiles
    if (helpFlag && aboutFlag) {
      println("atmost one of --help and --about arguments can be used.")
      None
    }
    else if (os.length > 1) {
      println("At most one of --shell, --noshell and --keepalive arguments can be used.")
      None
    } else {
      var fail = false
      fs.foreach { f =>
        val path = Paths.get(f)
        if (!Files.isRegularFile(path)) {
          println("Argument " + f + " cannot be used: " + path.toString + " is not a file.")
          fail = true
        }
      }
      if (fail || helpFlag) {
        usageMessage(toplevelArgs).foreach(println)
      }
      if (fail) None else Some(sa)
    }
  }
}
