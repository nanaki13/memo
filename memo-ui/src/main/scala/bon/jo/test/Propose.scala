package bon.jo.test

import bon.jo.html.DomShell.{ExtendedElement, ExtendedElmt, ExtendedHTMLCollection}
import bon.jo.html.HtmlEventDef._
import bon.jo.test.HTMLDef.{$attr, $attrns, $l, $ref, $refns, $va, HtmlOps}
import bon.jo.test.HtmlRep.ListRep
import org.scalajs.dom.html.{Button, Div}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{Element, HTMLElement, Node}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}


class Propose[A: HtmlRep, B <: raw.HTMLElement](
                                                 list: mutable.ListBuffer[A]
                                                 , val ioHtml: IOHtml[B, A]
                                                 , save: A => Future[Option[A]],
                                                 sel: A => Unit
                                               )(implicit executionContext: ExecutionContext) {


  val rep: HtmlRep[A] = implicitly[HtmlRep[A]]

  def addAll(a: IterableOnce[A]): mutable.ListBuffer[A] = {
    val l = a.iterator.toList

    l.foreach(b => {
      val h = rep.html(b)
      h.show(false)
      seleO.appendChild(h)
      h.$click { _ => sel(b) }
    })
    list.addAll(a)
  }


  val btn: Button = {
    val et = SimpleView.b("add")
    et._class = "btn btn-primary"
    et
  }


  private val seleO = {
    val l = $l div (list.html.toList)
    l.$attr( "class" -> "w-25")
  }

  val help: Element = {
    val r = ViewsDef.help.$attr("data-toggle" -> "tooltip", "data-delay" -> "500",
      "title" -> "Cliquer sur un tags pour l'ajouter au memo")
    r.show(false)
    jquery(r).tooltip()
    r
  }
  def html: Div = {
    val div: Div = $va.t div(ioHtml.html, btn, $va div (help, seleO))
    div
  }

  def read: Iterable[A] = list

  def +=(b: A): Node = {
    list += b
    val h = rep.html(b)
    seleO.appendChild(h)
    h.$click { _ => sel(b) }
    h
  }

  btn.$click {
    _ =>
      save(ioHtml.toValue).recover {
        case _ => None
      }.foreach {
        case None => PopUp("Marche pas...")
        case Some(value) => +=(value)
      }

  }


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
}

