package bon.jo.app

import bon.jo.html.HTMLDef.{$t, HtmlOps}
import bon.jo.rpg.ui.PlayerMessage
import org.scalajs.dom.window

trait SimpleMessage extends PlayerMessage {

  override type T = MEsageImpl

  val messageDiv = $t div "" := { d =>
    d.style.color = "white"
  }

  def message(str: String, timeToDisplay: Int): Unit = {
    val s = $t div (str)
    messageDiv.appendChild(s)
    lazy val t: Int = window.setTimeout(() => {
      window.clearTimeout(t)
      messageDiv.removeChild(s)
    }, timeToDisplay)
    t
  }

  def message(str: String): MEsageImpl = {
    val ret = MEsageImpl($t div (str))
    ret.str._class = "alert alert-warning"
    messageDiv.appendChild(ret.str)
    ret
  }

  def clear(str: MEsageImpl): Unit = {
    messageDiv.removeChild(str.str)
  }
}
