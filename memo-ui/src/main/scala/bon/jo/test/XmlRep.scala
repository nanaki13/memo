package bon.jo.test

import bon.jo.html.DomShell.{$, $c}
import bon.jo.memo.Dao.Id
import org.scalajs.dom.raw

import scala.xml.Node

object XmlRep {


  implicit class ListRep[A: XmlRep](seq: Iterable[A]) {
    def xml: Iterable[Node] = seq.map(_.xml)
  }

  implicit class PrXmlId[B: XmlRep](b: B) {
    def newHtml(implicit id : Id[B]): raw.Element = {
      val ret = $c[raw.Element](xml)
      ret.setAttribute("id", id.apply(b).toString)
      ret
    }

    def html(implicit id : Id[B]): raw.Element = $(id.apply(b).toString)

    def xml: Node = implicitly[XmlRep[B]].xml(b)
  }


  //def apply[A](a: A => Node)(implicit idp: Id[A]): IdXmlRep[A] = XmlRepImpl(a,idp)

  case class XmlRepImpl[A](xmlF : A => Node) extends XmlRep[A] {

    override def xml(memo: A): Node = xmlF(memo)


  }

//  trait IdXmlRep[A] extends XmlRep[A] {
//
//
//    def idtr: Id[A]
//  }

}

trait XmlRep[A] {
  def xml(memo: A): Node
 // def other[B](prefixId: String)(function: IdXmlRep[A] => B): B
}

