package bon.jo.test

import bon.jo.html.DomShell.{$, $c, clearAndAdd}
import bon.jo.memo.Dao.Id
import bon.jo.test.XmlRep.{IdXmlRep, XmlRepImpl}
import org.scalajs.dom.{console, raw}

import scala.xml.Node

object XmlRep {


  implicit class ListRep[A: IdXmlRep](seq: Iterable[A]) {
    def xml: Iterable[Node] = seq.map(_.xml)
  }

  implicit class PrXmlId[B: IdXmlRep](b: B) {
    def newHtml: raw.Element = {
      val ret = $c[raw.Element](xml)
      ret.setAttribute("id", implicitly[IdXmlRep[B]].idtr.id(b).toString)
      ret
    }

    def html: raw.Element = $(implicitly[IdXmlRep[B]].idtr.id(b).toString)

    def xml: Node = implicitly[IdXmlRep[B]].xml(b)
  }


  def apply[A](a: A => Node)(implicit idp: Id[A]): IdXmlRep[A] = XmlRepImpl(a,idp)

  case class XmlRepImpl[A](xmlF : A => Node,id: Id[A]) extends IdXmlRep[A] {
    override def idtr: Id[A] = id
    override def xml(memo: A): Node = xmlF(memo)

    override def other[B](prefixId: String)(function: IdXmlRep[A] => B): B  = {
      def nId : Id[A] = a => {
        val f = id.id _ andThen(v => s"$prefixId-$v")
        f(a)
      }
      function(copy(id = nId))
    }
  }

  trait IdXmlRep[A] extends XmlRep[A] {


    def idtr: Id[A]
  }

}

sealed trait XmlRep[A] {
  def xml(memo: A): Node
  def other[B](prefixId: String)(function: IdXmlRep[A] => B): B
}

