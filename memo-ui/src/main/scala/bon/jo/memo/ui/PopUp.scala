package bon.jo.memo.ui

import bon.jo.html.CommonHtml
import bon.jo.html.DomShell.ExtendedNode
import bon.jo.html.HTMLDef.{$c, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement

object PopUp {

  private def havePopupMessage = popCount > 0
  private def noPopuMessage = !havePopupMessage
  var popCount = 0
  lazy val popCnt = {
    val htmlNode : Div = $c.div
    htmlNode.style.position = "fixed"
    htmlNode.style.right = "10em"
    htmlNode.style.top = "2em"
    htmlNode._class = "card"
    htmlNode
  }
  private def show(htmlNode: HTMLElement): Unit = {
    document.body.appendChild(htmlNode)

  }


  def apply(message: String): Unit = {

    if (noPopuMessage) {
      show(popCnt)
    }
    popCount+=1
    val btn = CommonHtml.closeBtn
    btn.style.cssFloat = "right"
    val htmlNode : Div =  $va.t div( $va div btn,$t(message))
    htmlNode._class = "col"
    popCnt += htmlNode
    btn.$click { _ =>
      popCount -= 1
      htmlNode.removeFromDom()
      if(noPopuMessage){
        popCnt.removeFromDom()
      }
    }


  }
}
