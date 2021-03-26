package bon.jo.test

import org.scalajs.dom.html.{Button, Div, Input}
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable.ListBuffer
import scala.xml.Node
import XmlRep._
import SimpleView._

import scala.concurrent.Future
object SimpleView{
  type dc = DomCpnt[_ <: HTMLElement]
  type dci = DomCpnt[Input]

  def i: dci = DomCpnt[Input](id => <input id={id}></input>)

  def b(title: String): dc = DomCpnt[Button](id => <button id={id}>
    {title}
  </button>)
}
abstract case class SimpleView[A: IdXmlRep](creationHtml: () => Node) {

  def fillFromService: Future[Unit]
  val as: ListBuffer[A] = ListBuffer()
  val asHtml: dc = DomCpnt[Div](id => <div id={id}>
    {as.xml}
  </div>)
  val btnInput: dc = b("ajouter")
  val cpnt: DomCpnt[Div] = DomCpnt[Div](id => <div id={id}>
    {asHtml.xml}{creationHtml()}{btnInput.xml}
  </div>)

  def xml: Node = cpnt.xml

  def +=(p: A) = {

    asHtml.html.appendChild(p.newHtml)
    as += p
  }
}
