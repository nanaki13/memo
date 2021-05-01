package bon.jo.app

import bon.jo.game.html.Template
import org.scalajs.dom.html.Div

import scala.concurrent.Future

abstract class HtmlApp[Tp <: Template](template: Template) {
  def asynStartup(): Future[Unit] = Future.successful(())

  def haveAsynStartup: Boolean = false

  val user: User = template.user
  val typedTemplate: Tp = template.asInstanceOf[Tp]
}
