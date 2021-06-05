package bon.jo.html

import bon.jo.html.HTMLDef._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.html.HtmlRep.HtmlCpnt
import org.scalajs.dom.html.Button
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.collection.View.Single



object Selection {


  def selection[A, C <: HtmlCpnt](list: Iterable[A], target: HTMLElement, param: Param[A, C]): Future[Iterable[A]] =
    val res = Promise[Iterable[A]]()
    val buff = ListBuffer[A]()
    val cpnt = list map (e => (e, param.htmlRep.html(e)))
    cpnt map {
      case (a, c) => (a, c, $l span (c.list))
    } foreach {
      case (a, c, element) => element.$click { _ =>
        buff += a
        if param.multiple.isEmpty then
          res.success(buff.toList)
        else
          param.selListener(param.choisit.html(a))
          target.removeChild(element)

      }
      element.style.cursor = "pointer"
      target += element
      (a, c, element)
    }
    param.multiple match
      case Some(value) => value.$click(_ => res.success(buff.toList))
      case None =>
    res.future

  case class Param[A, C <: HtmlCpnt](
                                      multiple: Option[Button]

                                      , selListener: HtmlCpnt => Unit,
                                      htmlRep: HtmlRep[A, C],
                                      choisit: HtmlRep[A, C]
                                    ) :
    def selection(list: Iterable[A], target: HTMLElement): Future[Iterable[A]] = Selection.selection(list, target, this)

}