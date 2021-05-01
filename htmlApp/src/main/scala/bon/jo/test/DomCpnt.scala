package bon.jo.test

import java.util.UUID
import bon.jo.html.DomShell.$
import org.scalajs.dom.raw

import scala.xml.{Elem, Null, UnprefixedAttribute}

trait DomCpnt[H <: raw.Element] {

  val id: String = UUID.randomUUID().toString

  def html: H = $[H](id)

  def xml: Elem
}

object DomCpnt {
  class Impl[H<: raw.Element](xmlp : Elem) extends DomCpnt[H ] {

    override def xml: Elem =  xmlp.copy(attributes = xmlp.attributes.append(new UnprefixedAttribute("id",id,Null)))
  }

//  def apply[E <: raw.Element](iderXml: String => Elem): DomCpnt[E] = new DomCpnt[E] {
//    override def xml: Elem = iderXml(id)
//  }
  def apply[E <: raw.Element](xmlp : Elem): DomCpnt[E] = new Impl[E](xmlp)
}