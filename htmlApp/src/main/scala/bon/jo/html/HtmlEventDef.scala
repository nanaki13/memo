package bon.jo.html

import org.scalajs.dom.raw.{Event, FocusEvent, HTMLElement, KeyboardEvent, MouseEvent}

import scala.scalajs.js


object HtmlEventDef {

  trait CaseEvent[A <: Event] {
    def apply(html: HTMLElement, a: A => Unit): js.Function1[A , _] = {
      val f :  js.Function1[A, _] = a
      html.addEventListener(toString, f)
       f
    }
  }

  case object blur extends CaseEvent[FocusEvent]

  case object click extends CaseEvent[MouseEvent]
  case object keyup extends CaseEvent[KeyboardEvent]
  case object change extends CaseEvent[Event]

  implicit class ExH(val html: HTMLElement) extends HtmlEventDef

}

trait HtmlEventDef {
  val html: HTMLElement

  def $blur: (FocusEvent => Unit) =>  js.Function1[FocusEvent, _] = HtmlEventDef.blur(html, _)
  def $click: (MouseEvent => Unit) => js.Function1[MouseEvent, _] = HtmlEventDef.click(html, _)

  def $keyup: (KeyboardEvent => Unit) => js.Function1[KeyboardEvent, _]  = HtmlEventDef.keyup(html, _)
  def $change: (Event => Unit) =>  js.Function1[Event, _] = HtmlEventDef.change(html, _)
  def $Action(doThis: => Unit): Unit = {
    $keyup{e => if(e.keyCode == 13) doThis}
  }

}

