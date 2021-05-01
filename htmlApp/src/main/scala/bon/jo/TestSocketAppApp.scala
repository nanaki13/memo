package bon.jo


import bon.jo.app.HtmlApp
import bon.jo.game.html.Template
import bon.jo.util.SocketKeeper
import bon.jo.util.SocketKeeper.SocketContext
import org.scalajs.dom.html.Div

class TestSocketAppApp(app: Div, template: Template) extends HtmlApp[TestSocketTemplate](template: Template) {
  typedTemplate.myButton.obsClick().suscribe(_ => {
    val p = typedTemplate.getParam
    implicit val sc: SocketContext = SocketContext("send", p.url, e => {
      typedTemplate.msg(e.data.toString)
    })
    SocketKeeper.send(p.send)
  })
}
