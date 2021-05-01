package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.test.HtmlRep.{HtmlCpnt, HtmlRepParam}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement

import scala.util.Try



object HtmlRep {

  def apply[A](f : A => HtmlCpnt):HtmlRep[A] = new HtmlRep[A] {
    override def htmlt(memo: A, p: Option[Nothing]): HtmlCpnt with TypedHtml[A] = new HtmlCpnt with TypedHtml[A]{
      override val get: IterableOnce[HTMLElement] = f(memo).list

      override def update(value: Option[A]): Unit = {

      }
    }
  }

  implicit class ListRep[A: HtmlRep](seq: Iterable[A]) {
    def html: Iterable[HtmlCpnt] = seq.map(_.html)
  }

  implicit class PrXmlId[B](b: B) {

    def html(implicit v: HtmlRep[B]): HtmlCpnt = implicitly[HtmlRep[B]].html(b)

    def htmlp[P](p: Option[P])(implicit v: HtmlRepParam[B, P]): HtmlCpnt = v.html(b, p)
    def htmlpt[P](p: Option[P])(implicit v: HtmlRepParam[B, P]): Typed[B]= v.htmlt(b, p)
    def typedHtml[P,C <: HtmlCpnt](p: Option[P])(implicit v: HtmlRepParam[B, P],cv : HtmlCpnt => C): C= v.htmlt(b, p).cast[C]
    def htmlp[P](p: P)(implicit v: HtmlRepParam[B, P]): HtmlCpnt = v.html(b, Some(p))
  }


  type Typed[A] = HtmlCpnt with  TypedHtml[A]
  trait HtmlRepParam[A, P] {


    def html(memo: A, p: Option[P]): HtmlCpnt = htmlt(memo,p)
    def htmlt(memo: A, p: Option[P]): Typed[A]
  }

  trait HtmlCpnt{

    def get : IterableOnce[HTMLElement]
    def list: List[HTMLElement] = get.iterator.toList
    def head: HTMLElement = list.head

    def cast[A <: HtmlCpnt](implicit cv : HtmlCpnt => A):A = cv(this)

  }
  trait TypedHtml[T]{


    def update(value: Option[T]): Unit

  }
  object HtmlCpnt{
    def apply[T <: IterableOnce[HTMLElement]](f : ()=>T) : HtmlCpnt = {
      new HtmlCpnt {
        override val get: IterableOnce[HTMLElement] = f()
      }
    }
    implicit class FToHtmlCpnt[T <:HTMLElement](f : () =>T){
        def toHtmlCpnt: HtmlCpnt = {
          HtmlCpnt.apply[Some[HTMLElement]](() => Some(f()))
        }
    }
    implicit class FLToHtmlCpnt[T <:IterableOnce[HTMLElement]](f : () =>T){
      def toHtmlCpnt: HtmlCpnt = {
        HtmlCpnt.apply[IterableOnce[HTMLElement]](() => f())
      }
    }
  }
}

trait HtmlRep[A] extends HtmlRepParam[A, Nothing] {
  def html(memo: A): HtmlCpnt = html(memo, None)
}

