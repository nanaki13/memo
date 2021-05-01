package bon.jo.app

import bon.jo._
import bon.jo.game.html.Template
import bon.jo.test.{Test, TestTemplate}
import org.scalajs.dom.document
import org.scalajs.dom.html.Div



object AppLoaderExample extends App with AppLoader{

  val apps = List("app-test-socket", "app-test")

  val conf: Map[String, HtmlAppFactory[_]] = Map(
    "app-test-socket" -> new HtmlAppFactory[TestSocketTemplate]((app: Div, template: Template) => new TestSocketAppApp(app, template), _ => new TestSocketTemplate),
    "app-test" -> new HtmlAppFactory[TestTemplate]((app: Div, template: Template) => new Test(template), q =>  TestTemplate(user = q))
  )
  loads(apps)

}


