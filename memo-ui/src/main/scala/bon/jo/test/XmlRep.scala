package bon.jo.test

import bon.jo.html.DomShell.{$, $c}
import bon.jo.memo.Dao.Id
import org.scalajs.dom.{console, raw}

import scala.xml.{Group, Node}

object XmlRep {

  implicit class ListRep[A: IdXmlRep](seq: Iterable[A]) {
    def xml: Iterable[Node] = seq.map(_.xml)
  }

//  implicit class PrXml[B: XmlRep](b: B ) {
//    def xml: Node = implicitly[XmlRep[B]].xml(b)
//
//    def newHtml: raw.Element = $c(xml)
//  }

    implicit class PrXmlId[B : IdXmlRep ](b: B) {
      def newHtml : raw.Element = {
        val ret = $c[raw.Element](xml)
        ret.setAttribute("id",implicitly[IdXmlRep[B]].idtr.id(b).toString)
        ret
      }
      def html: raw.Element = $(implicitly[IdXmlRep[B]].idtr.id(b).toString)
      def xml: Node = implicitly[IdXmlRep[B]].xml(b)
    }




  def apply[A](a: A => Node)(implicit idp: Id[A]): IdXmlRep[A] = new IdXmlRep[A] {
    console.log("apply"+idp)
    override def xml(memo: A): Node = a(memo)

    override val idtr : Id[A] = idp
  }



}

trait XmlRep[A] {
  def xml(memo: A): Node
}
trait IdXmlRep [A] extends XmlRep[A] {
  val idtr : Id[A]
}
