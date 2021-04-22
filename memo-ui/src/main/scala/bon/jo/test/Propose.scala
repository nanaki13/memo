package bon.jo.test

import bon.jo.html.HtmlEventDef._
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.memo.Dao.Id
import bon.jo.test.HTMLDef.{$l, $t, $va_t, HtmlOps}
import bon.jo.test.HtmlRep.ListRep
import org.scalajs.dom.html.{Button, Div}
import org.scalajs.dom.{console, raw, window}
import org.scalajs.dom.raw.{Event, HTMLElement, Node}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("$")
object jquery extends scalajs.js.Any {
  def apply(str: String): scalajs.js.Dynamic = js.native

  def apply(str: scalajs.js.Any): scalajs.js.Dynamic = js.native
}

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
    l.$attr("data-toggle" -> "tooltip", "data-delay" -> "500",
      "title" -> "Cliquer sur un tags pour l'ajouter au memo", "data-placement" -> "left")
    jquery(l).tooltip()
    l
  }

  def html: Div = {
    val div: Div = $va_t div(ioHtml.html, btn, seleO)

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

    (list zip seleO.children).foreach(a => {
      val h = a._2.asInstanceOf[HTMLElement]
      if (filter(a._1)) {
        h.show(true)
      } else {
        h.show(false)
      }
    })
  }
}

