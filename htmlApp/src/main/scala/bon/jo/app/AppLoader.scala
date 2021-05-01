package bon.jo.app

import bon.jo.Logger
import bon.jo.game.html.Template
import bon.jo.html.DomShell.{$, ExtendedHTMLCollection}
import bon.jo.html.util.Anim
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.Element

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait AppLoader {

  /**
   * get the conf for an app and inject html from body template in element.
   * When it's done, call afterInDom on the template
   *
   * @param app
   * @param element
   * @return
   */
  def loadApp(app: String, element: Element, user: User = User.Visitor): HtmlApp[Template] = {
    val confo: HtmlAppFactory[Template] = conf(app).asInstanceOf[HtmlAppFactory[Template]]
    val htmlFact = confo.htmlAppFactory
    val templateFact = confo.templateFactory
    val template = templateFact(user)
    element.innerHTML = template.body
    val appDiv: Div = $(template.id)
    val ret = htmlFact(appDiv, template)
    if (ret.haveAsynStartup) {
      Logger.log(s"start asynchrone $app")
      (ret.asynStartup() map {
        _ =>
          Logger.log(s"after load process asynchrone , init template $app")
          template.init(appDiv)
      }).onComplete {
        case Failure(exception) => Logger.log(s"asynchrone loading $app FAIL  :$exception, ${exception.getMessage}"); throw exception
        case Success(_) => Logger.log(s"asynchrone loading $app OK")
      }
    } else {
      Logger.log(s"start normal  , init template  $app")
      template.init(appDiv)
    }

    ret
  }


  /**
   * find the apps in html and load it
   *
   * @param apps
   */
  def loadsWithUser(apps: List[String])(user: User): List[HtmlApp[Template]] = {
    for {app <- apps
         appInit <- document.getElementsByTagName(app)
         } yield {
      loadApp(app, appInit, user)
    }
  }

  /**
   * find the apps in html and load it
   *
   * @param apps
   */
  def loads(apps: List[String]): Unit = {
    for (app <- apps) {
      val appInit = document.getElementsByTagName(app)
      if (appInit != null && appInit.nonEmpty) {
        loadApp(app, appInit(0))
      }
    }
    Anim.start()
  }

  def loadWithAuth(apps: List[String]): Future[List[HtmlApp[Template]]] = {

    Auth.doAuth() map loadsWithUser(apps)

  }


  val conf: Map[String, HtmlAppFactory[_]]
  val apps: List[String]
}
