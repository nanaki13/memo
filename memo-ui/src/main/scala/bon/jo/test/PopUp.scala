package bon.jo.test

import bon.jo.html.DomShell.$
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.test.HTMLDef.{$ref, $t, $va}
import org.scalajs.dom.document
import org.scalajs.dom.html.{Button, Div}
import org.scalajs.dom.raw.HTMLElement

object PopUp {

  private var _show = false

  private def show(htmlNode: HTMLElement): Unit = {
    document.body.appendChild(htmlNode)
    _show = true
  }

  private def hide(htmlNode: HTMLElement): Unit = {
    document.body.removeChild(htmlNode)
    _show = false
  }

  def apply(message: String): Unit = {
    if (!_show) {
      def xml = <div>
        {message}<div>
          <button id="pidb">X</button>
        </div>
      </div>

      val btn = ViewsDef.closeBtn
      val htmlNode : Div = $va.t div ($t(message),  $va.t div btn)
      htmlNode.style.position = "fixed"
      htmlNode.style.right = "10em"
      htmlNode.style.top = "2em"
      show(htmlNode)
      btn.$click { _ => hide(htmlNode) }
    }

  }
}
