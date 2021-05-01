package bon.jo.html

import DomShell.$c
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{Element, HTMLCollection}

import scala.xml.{Node, NodeBuffer}

object BridgeXmlHtml {


   def toElement[E <: Element](n: Node): E = {
    $c[E](n.mkString)
  }

   def toElementBase(n: NodeBuffer): HTMLCollection = {
    $c[Div](n.mkString).children
  }


}
