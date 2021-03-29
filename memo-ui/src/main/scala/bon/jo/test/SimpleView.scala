package bon.jo.test

import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node}
import XmlRep._
import SimpleView._
import bon.jo.memo.Entities
import org.scalajs.dom.raw

import scala.concurrent.Future
object SimpleView{
  trait DSelect{
    self : DomCpnt[Select] =>
    def selectFirst = html.firstElementChild.asInstanceOf[raw.HTMLOptionElement].selected = true
  }
  type dc = DomCpnt[_ <: HTMLElement]
  type dci = DomCpnt[Input]
  type dcta = DomCpnt[TextArea]
  type dcselect = DomCpnt[Select] with DSelect
  def i: dci = DomCpnt[Input](id => <input id={id}></input>)
  def ta: dcta = DomCpnt[TextArea](id => <textarea  cols="150"  rows="50" id={id}></textarea>)
  def b(title: String): dc = DomCpnt[Button](id => <button id={id}>
    {title}
  </button>)
  def s[A](elemnts : Iterable[A])(implicit kv : (A => String,A => String)): dcselect =

    new DomCpnt[Select] with DSelect {
      override def xml: Elem = <select id={id}>
        {elemnts.map(el => <option value={kv._1(el)}>{kv._2(el)}</option>)}
      </select>
    }


  def sv[A](implicit elemnts : Entities.EnumComp[A],  v : A => String):dcselect = s(elemnts.values)( (k =>k.toString,a => v(a)) )
  def s[A](implicit elemnts : Entities.EnumComp[A]): dcselect = s(elemnts.values)( (k =>k.toString, a => a.toString) )
}
abstract  class SimpleView[A: IdXmlRep](creationHtml: () => Node) {

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
