package bon.jo.memo.ui

import bon.jo.html.DomShell.{ExtendedElement, ExtendedElmt, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef._
import bon.jo.html.HtmlEventDef._
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.HtmlCpnt
import org.scalajs.dom.html.{Button, Div}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{Element, HTMLElement, MouseEvent}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.scalajs.js


class Propose[A, B <: raw.HTMLElement](
                                                 val ioHtml: IOHtml[B, A],
                                                 val proposeView: ProposeView[A,_]
                                               )(implicit executionContext: ExecutionContext) {







  val btn: Button = {
    SimpleView.bsButton("add")

  }





  val html: Div = {
    val div: Div = $va.t div(ioHtml.html, btn)
    div
  }


//  type Ev = Iterable[js.Function1[MouseEvent, _]]
  def focus():Unit = html += proposeView.html
  def createAllEvent(d : A=>Unit):Ev = {
    proposeView.seleO.children zip  proposeView.list map {
      case (element, a) =>  (element,clickEvent(element.asInstanceOf[HTMLElement], a)(d))
    }

  }
  def clickEvent(htmlp : HTMLElement,b : A)(sel : A=>Unit): js.Function1[MouseEvent, _] =  htmlp.$click { _ => sel(b) }







}

object ProposeView{
  def help: Element = {

    val r = ViewsDef.help.$attr("data-toggle" -> "tooltip", "data-delay" -> "500",
      "title" -> "Cliquer sur un tags pour l'ajouter au memo")
    r.show(false)
    jquery(r).tooltip()
    r
  }
}

case class ProposeView[A,C<:HtmlCpnt](
                        seleO : HTMLElement = $c.div ,
                        help : Element = ProposeView.help,
                          list: mutable.ListBuffer[A] =  mutable.ListBuffer.empty[A],
                      )(implicit rep  : HtmlRep[A,C]){

  val html: HTMLElement = $va   div (help, seleO)
  def doFilter(filter: A => Boolean): Unit = {

    val s =  (list zip seleO.children).map(a => {
      val h = a._2.asInstanceOf[HTMLElement]
      if (filter(a._1)) {
        h.show(true);1
      } else {
        h.show(false);0
      }
    }).sum
    help.show(s > 0)
  }

  def addAll(a: IterableOnce[A]): mutable.ListBuffer[A] = {
    val l = a.iterator.toList

    l.foreach(b => {
      val h = rep.html(b).list
      h foreach(_.show(false))
      seleO ++= h

    })
   list.addAll(a)
  }
  def +=(b: A): List[HTMLElement] = {
    list += b
    val h = rep.html(b)
    //val html = wrap(h)
    seleO ++= h.list

    h.list
  }
  def wrap(a :HtmlCpnt) =  $ref div{ d => d ++= a.list}
}