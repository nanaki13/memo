package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.test.HtmlRep.{HtmlCpnt, HtmlRepParam}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement



object HtmlRep {

  def apply[A](f : A => HtmlCpnt):HtmlRep[A] = (a,_)=>f(a)

  implicit class ListRep[A: HtmlRep](seq: Iterable[A]) {
    def html: Iterable[HtmlCpnt] = seq.map(_.html)
  }

  implicit class PrXmlId[B](b: B) {

    def html(implicit v: HtmlRep[B]): HtmlCpnt = implicitly[HtmlRep[B]].html(b)

    def htmlp[P](p: Option[P])(implicit v: HtmlRepParam[B, P]): HtmlCpnt = v.html(b, p)
    def htmlp[P](p: P)(implicit v: HtmlRepParam[B, P]): HtmlCpnt = v.html(b, Some(p))
  }


  trait HtmlRepParam[A, P] {

    def html(memo: A, p: Option[P]): HtmlCpnt

  }

  trait HtmlCpnt{
    def get : IterableOnce[HTMLElement]
    def list: List[HTMLElement] = get.iterator.toList
    def head: HTMLElement = list.head
  }
  object HtmlCpnt{
    def apply[T <: IterableOnce[HTMLElement]](f : ()=>T) : HtmlCpnt = {
      new HtmlCpnt {
        override def get: IterableOnce[HTMLElement] = f()
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

