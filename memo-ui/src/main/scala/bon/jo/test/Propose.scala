package bon.jo.test

import bon.jo.html.DomShell.{ExtendedElement, ExtendedElmt, ExtendedHTMLCollection}
import bon.jo.html.HtmlEventDef._
import bon.jo.test.HTMLDef.{$attr, $attrns, $c, $l, $ref, $refns, $va, Ev, HtmlOps}
import bon.jo.test.HtmlRep.{HtmlCpnt, ListRep}
import org.scalajs.dom.html.{Button, Div}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{Element, HTMLElement, MouseEvent, Node}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js


class Propose[A: HtmlRep, B <: raw.HTMLElement](
                                                 val ioHtml: IOHtml[B, A]
                                                 , save: A => Future[Option[A]],
                                                 val proposeView: ProposeView[A]
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



  btn.$click {
    _ =>
      save(ioHtml.toValue).recover {
        case _ => None
      }.foreach {
        case None => PopUp("Marche pas...")
        case Some(value) => proposeView.+=(value)
      }

  }



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

case class ProposeView[A: HtmlRep](
                        seleO : HTMLElement = $c.div ,
                        help : Element = ProposeView.help,
                          list: mutable.ListBuffer[A] =  mutable.ListBuffer.empty[A],

                      ){
  val rep: HtmlRep[A] = implicitly[HtmlRep[A]]
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
  def +=(b: A): Iterable[HTMLElement] = {
    list += b
    val h = rep.html(b)
    val html = wrap(h)
    seleO += html

    html.children.map(_.asInstanceOf[HTMLElement])
  }
  def wrap(a :HtmlCpnt) =  $ref div{ d => d ++= a.list}
}