package bon.jo.app

import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef.{$c, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.stat.Perso.WithUI
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{console, window}

import scala.concurrent.ExecutionContext.Implicits.global

class TimeLineCpnt(val el: TimeLineParam, val withUI: WithUI) {

  import withUI.acImpl

  implicit val ui = withUI.o.ui
  val tlView: Div = $c.div
  tlView.draggable = true


  tlView.style.position = "absolute"
  tlView.style.top = "10px"
  tlView.style.right = s"0"

  val htmlName = el.timedObjs.map(_.simpleName).map(t => $t span t)
  htmlName.map {
    e =>

      e.style.position = "absolute"
      e
  }.foreach(e => tlView.appendChild({
    val in = $va div (e)
    in.style.height = "1em"
    val s1: Span = $c.span
    s1.style.width = s"${el.chooseAction}px"
    s1.style.backgroundColor = "blue"
    s1.style.height = "1em"
    s1.style.display = "inline-block"
    val s2: Span = $c.span
    s2.style.width = s"${el.action - el.chooseAction}px"
    s2.style.backgroundColor = "red"
    s2.style.height = "1em"
    s2.style.display = "inline-block"
    val s3: Span = $c.span
    s3.style.width = s"12em"
    s3.style.backgroundColor = "green"
    s3.style.opacity = "0"
    s3.style.height = "1em"
    s3.style.display = "inline-block"
    in ++= (s1, s2, s3)
    in
  }))

  def update = {
    htmlName zip el.timedObjs foreach {
      case (element, value) =>
        element.style.left = value.pos.toString + "px"
    }
  }

  def doEvent() = {
    lazy val int: Int = window.setInterval(() => {
      if (el.pause == 0) {
        el.nextState
        update
      } else {
        window.clearInterval(int)
      }

      //    cpnt.foreach {
      //      case (perso, cpnt) => cpnt.update(Some(perso))
      //    }
    }, 25)
    int

  }

  el.resume = doEvent _

}
