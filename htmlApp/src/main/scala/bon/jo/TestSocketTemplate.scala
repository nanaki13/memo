package bon.jo

import bon.jo.app.User
import bon.jo.game.html.Template
import bon.jo.html.OnClick
import bon.jo.html.DomShell.{$, inputXml}
import bon.jo.html.OnClick.ButtonType
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.HTMLElement

class TestSocketTemplate(override val user: User = User.Visitor) extends Template {
  var msg: Div = _
  var urlInput: Input = _
  var sendInput: Input = _

   def updateView(): Unit = {
    myButton.addTo("submit")
    msg = $[Div]("resp")
    urlInput = $[Input]("url")
    sendInput = $[Input]("send")
  }

  override def body: String = <div id="msg" class="container">
    {inputXml("url", "url", "Const.urlCardGame")}{inputXml("send", "envoi")}<div id="submit">

    </div>
    <div>
      <div  id="resp" class="container">

      </div>
    </div>
  </div>.mkString

  val myButton: ButtonType = OnClick("send-button", "Envoi")


  def msg(str: String): Unit = {
    msg.innerHTML = s"""<span class="m-2">reponse</span><pre>"""+ str+"</pre>" + msg.innerHTML
  }

  case class Param(url: String, send: String)

  def getParam: Param = {
    Param(urlInput.value, sendInput.value)
  }

  override def init(parent : HTMLElement): Unit = {}
}
