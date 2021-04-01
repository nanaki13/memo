package bon.jo.test

import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement}

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node}
import XmlRep._
import SimpleView._
import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import org.scalajs.dom.raw

import scala.concurrent.Future
object SimpleView{
  trait DSelect{
    self : DomCpnt[Select] =>
    def selectFirst(): Unit = html.firstElementChild.asInstanceOf[raw.HTMLOptionElement].selected = true
    def select(v : String): Unit = html.getElementsByTagName("option").map(_.asInstanceOf[HTMLOptionElement]).filter(_.value == v).foreach(_.selected = true)
  }
  type dc = DomCpnt[_ <: HTMLElement]
  type dci = DomCpnt[Input]
  type dcta = DomCpnt[TextArea]
  type dcselect = DomCpnt[Select] with DSelect
  def i: dci = DomCpnt[Input](<input></input>)
  def ta: dcta = DomCpnt[TextArea](<textarea  cols="150"  rows="51" ></textarea>)
  def b(title: String): dc = DomCpnt[Button]( <button >
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

abstract  class SimpleView[A: XmlRep](creationHtml: () => Node)(implicit ida : Id[A]) {

  def fillFromService: Future[Unit]
  val as: ListBuffer[A] = ListBuffer()
  val asHtml: dc = DomCpnt[Div](<div>
    {as.xml}
  </div>)
  val btnInput: dc = b("ajouter")
  val cpnt: DomCpnt[Div] = DomCpnt[Div](<div>
    {asHtml.xml}{creationHtml()}{btnInput.xml}
  </div>)

  val idaPr : Id[A] = ida.prefix(cpnt.id)
  def xml: Node = cpnt.xml

  def +=(p: A): ListBuffer[A] = {
    asHtml.html.appendChild(p.newHtml(idaPr,implicitly[XmlRep[A]]))
    as += p
  }
}
