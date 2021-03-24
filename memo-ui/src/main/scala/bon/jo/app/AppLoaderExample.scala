package bon.jo.app

import bon.jo._
import bon.jo.game.html.Template
import bon.jo.test.{MemoApp, MemoTemplate}
import org.scalajs.dom.document
import org.scalajs.dom.html.Div

import scala.concurrent.ExecutionContext.Implicits._

object AppLoaderExample extends App with AppLoader{

  val apps = List("app-test-socket", "app-test")

  val conf: Map[String, HtmlAppFactory[_]] = Map(
    "app-test-socket" -> new HtmlAppFactory[TestSocketTemplate]((app: Div, template: Template) => new TestSocketAppApp(app, template), _ => new TestSocketTemplate),
    "app-test" -> new HtmlAppFactory[MemoTemplate]((app: Div, template: Template) => new MemoApp(app, template), q =>  MemoTemplate(user = q))
  )
  loads(apps)

}


