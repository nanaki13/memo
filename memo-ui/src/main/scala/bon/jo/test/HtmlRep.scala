package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.test.HtmlRep.XmlRepParam
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement



object HtmlRep {

  def apply[A](f : A => HTMLElement):HtmlRep[A] = (a,_)=>f(a)

  implicit class ListRep[A: HtmlRep](seq: Iterable[A]) {
    def html: Iterable[HTMLElement] = seq.map(_.html)
  }

  implicit class PrXmlId[B](b: B) {
    def newHtml(implicit id: Id[B], v: HtmlRep[B]): raw.HTMLElement = {
      val ret = html
      ret.id = id.apply(b).toString
      ret
    }


    def html(implicit v: HtmlRep[B]): HTMLElement = implicitly[HtmlRep[B]].html(b)

    def htmlp[P](p: Option[P])(implicit v: XmlRepParam[B, P]): HTMLElement = v.html(b, p)

  }


  trait XmlRepParam[A, P] {

    def html(memo: A, p: Option[P]): HTMLElement

  }
}

trait HtmlRep[A] extends XmlRepParam[A, Nothing] {
  def html(memo: A): HTMLElement = html(memo, None)
}

