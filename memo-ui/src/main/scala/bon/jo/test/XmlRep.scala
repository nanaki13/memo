package bon.jo.test

import bon.jo.html.DomShell.{$, $c}
import bon.jo.memo.Dao.Id
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement

//import scala.xml.Node

object XmlRep {


  implicit class ListRep[A: XmlRep](seq: Iterable[A]) {
    def xml: Iterable[HTMLElement] = seq.map(_.html)
  }

  implicit class PrXmlId[B](b: B) {
    def newHtml(implicit id : Id[B],v : XmlRep[B]): raw.HTMLElement = {
      val ret = html
      ret.id =  id.apply(b).toString
      ret
    }



    def html(implicit v : XmlRep[B]): HTMLElement = implicitly[XmlRep[B]].html(b)
    def html[C](f : Option[C] => Unit)(implicit v : XmlRepCapt[B,C]): HTMLElement = v.html(b,f)
  }


  //def apply[A](a: A => Node)(implicit idp: Id[A]): IdXmlRep[A] = XmlRepImpl(a,idp)

  case class XmlRepImpl[A](xmlF : A => HTMLElement) extends XmlRep[A] {

    override def html(memo: A): HTMLElement = xmlF(memo)


  }

//  trait IdXmlRep[A] extends XmlRep[A] {
//
//
//    def idtr: Id[A]
//  }

}

trait XmlRep[A] {
  def html(memo: A): HTMLElement

 // def other[B](prefixId: String)(function: IdXmlRep[A] => B): B
}
trait XmlRepCapt[A,B]  extends XmlRep[A]{
  def html(memo: A,argCapt : Option[B] => Unit): HTMLElement
  def html(memo: A): HTMLElement = html(memo, _ =>{})
  // def other[B](prefixId: String)(function: IdXmlRep[A] => B): B
}

