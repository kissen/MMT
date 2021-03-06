package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.web.RemoteAdminServer

/** Shared base class for Actions that are meta and do not actually do something */
sealed abstract class MetaAction extends Action {}

case object NoAction extends MetaAction {
  def apply() {}
  def toParseString = "noop"
}
object NoActionCompanion extends ObjectActionCompanion(NoAction, "do nothing", "noop")

/**
  * run an action on a remotely administered client
  *
  * concrete syntax: remote id:STRING ACTION
  */
case class RemoteAction(id: String, action: Action) extends MetaAction {
  def apply() {
    controller.extman.get(classOf[RemoteAdminServer]).headOption match {
      case None => controller.report("error", "no admin server loaded")
      case Some(ras) => ras(this)
    }
  }
  def toParseString = s"remote $id $action"
}
object RemoteActionCompanion extends ActionCompanion("run an action on a remotely administered client", "remote") {
  import Action._
  def parserActual(implicit state: ActionState) = str ~ action(state) ^^ {case id ~ act => RemoteAction(id, act)}
}
