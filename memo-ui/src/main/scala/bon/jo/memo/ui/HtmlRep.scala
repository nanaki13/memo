package bon.jo.memo.ui

import bon.jo.memo.Dao.Id
import HtmlRep.{HtmlCpnt, HtmlRepParam}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement
import HTMLDef.HtmlOps
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao

import scala.concurrent.ExecutionContext
import scala.util.Try


object HtmlRep {


  implicit class ListRep[A, C <: HtmlCpnt](seq: Iterable[A])(implicit v: HtmlRep[A, C]) {
    def html: Iterable[HtmlCpnt] = seq.map(_.html)
  }

  implicit class PrXmlId[B](b: B) {

    def html[C <: HtmlCpnt](implicit v: HtmlRep[B, C]): C = v.html(b)

    def htmlp[P, C <: HtmlCpnt](p: Option[P])(implicit v: HtmlRepParam[B, P, C]): C = v.html(b, p)

    def htmlp[P, C <: HtmlCpnt](p: P)(implicit v: HtmlRepParam[B, P, C]): C = v.html(b, Some(p))
  }


  trait HtmlRepParam[A, P, C <: HtmlCpnt] {
    def html(memo: A, p: Option[P]): C
  }


  trait HtmlCpnt {

    def get: IterableOnce[HTMLElement]

    def list: List[HTMLElement] = get.iterator.toList

    def head: HTMLElement = list.head

    //  def cast[A <: HtmlCpnt](implicit cv : HtmlCpnt => A):A = cv(this)

  }

  trait UpdatableHtmlCpnt[T] {
    def update(value: Option[T]): Unit = {}
  }

  trait Deletable {
    selef: HtmlCpnt =>
    implicit val executionContext: ExecutionContext
    val deleteButton = ViewsDef.closeBtn
    deleteButton.$click{ _=>
      doDelete
    }
    def delete(): Dao.FB
    var postDelete: List[()=>Unit] = Nil
    def doDelete: Dao.FB = {
      delete().map { r =>
        if (r) {
          list.foreach { z =>
            z.safeRm()
          }
          postDelete.foreach(_())
          true
        } else {
          false
        }
      }
    }
  }
  object HtmlCpnt {
    def apply[T <: IterableOnce[HTMLElement]](f: () => T): HtmlCpnt = {
      new HtmlCpnt {
        override val get: IterableOnce[HTMLElement] = f()
      }
    }

    implicit class FToHtmlCpnt[T <: HTMLElement](f: () => T) {
      def toHtmlCpnt: HtmlCpnt = {
        HtmlCpnt.apply[Some[HTMLElement]](() => Some(f()))
      }
    }

    implicit class FLToHtmlCpnt[T <: IterableOnce[HTMLElement]](f: () => T) {
      def toHtmlCpnt: HtmlCpnt = {
        HtmlCpnt.apply[IterableOnce[HTMLElement]](() => f())
      }
    }

  }

}



trait HtmlRep[A, C <: HtmlCpnt] extends HtmlRepParam[A, Nothing, C] {
  override def html(memo: A, p: Option[Nothing]): C = html(memo)

  def html(memo: A): C
}

