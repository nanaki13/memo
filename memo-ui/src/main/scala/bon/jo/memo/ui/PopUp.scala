package bon.jo.memo.ui

import bon.jo.html.CommonHtml
import bon.jo.html.DomShell.{ExtendedHTMLCollection, ExtendedNode}
import bon.jo.html.HTMLDef.{$c, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{HTMLElement, Node}

object PopUp {

  private def havePopupMessage = popCount > 0
  private def noPopuMessage = !havePopupMessage
  var popCount = 0
  lazy val popCnt: Div = {
    val htmlNode : Div = $c.div
    htmlNode.style.position = "fixed"
    htmlNode.style.right = "10em"
    htmlNode.style.top = "2em"
    htmlNode.style.paddingRight = "2em"
    htmlNode.style.maxWidth="20em"
    htmlNode._class = "card m-5"
    htmlNode.style.zIndex="1001"
    htmlNode.draggable = true
    htmlNode.$userCanDrag()
    htmlNode
  }
  private def show(htmlNode: HTMLElement): Unit = {
    document.body.appendChild(htmlNode)

  }
  def apply(message: Node): Unit = {
    if (noPopuMessage) {
      show(popCnt)
    }
    popCount+=1
    val btn = CommonHtml.closeBtn
    btn.classList add "popup-close"
    val htmlNode : Div =  $va.t div List( $va div (btn,message).toList )

    popCnt += htmlNode
    btn.$click { _ =>
      popCount -= 1
      htmlNode.removeFromDom()
      if(noPopuMessage){
        popCnt.removeFromDom()
      }
    }
    if(popCount>10){
      popCnt.removeChild( popCnt.children.head)
      popCount -= 1
    }

  }

  def apply(message: String): Unit = {
    apply($t(message))


  }
}
