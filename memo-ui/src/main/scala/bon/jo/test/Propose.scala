package bon.jo.test
import bon.jo.html.DomShell.$
import bon.jo.html.HtmlEventDef._
import bon.jo.memo.Dao.Id
import bon.jo.test.XmlRep._
import bon.jo.test.XmlRep.ListRep
import org.scalajs.dom.html.{Div, Select}
import org.scalajs.dom.{console, raw}
import org.scalajs.dom.raw.{HTMLElement, Node}

import scala.collection.mutable
import scala.xml.Elem
trait HtmlExtract[A,B<: raw.Element]{
    def extract(html : B) : A
}
trait HtmlExtractAny[A] extends HtmlExtract[A, raw.Element]

class IOHtml[A <: raw.Element,E](xmlf :String  => Elem,extractF: A => E) extends DomCpnt[A] with HtmlExtract[E,A] {
  override def xml: Elem = xmlf(id)

  override def extract(htmlp: A): E = extractF(htmlp)
  def toValue: E = extract(html)
}
object HtmlExtract{
  implicit class HtmlValue[A <: raw.Element,B : HtmlExtractAny](a : A){
      def toValue(): B = implicitly[HtmlExtractAny[B]].extract(a)
  }
}

class Propose[A :IdXmlRep,B <:raw.HTMLElement]( list: mutable.ListBuffer[A],val ioHtml: IOHtml[B,A]) extends DomCpnt[Div] {



  def addAll (a : IterableOnce[A])= {
    val l = a.iterator.toList

    l.foreach(b => {
      val h  = b.newHtml
      h.asInstanceOf[HTMLElement].style.display = "none"
      seleO.appendChild(h)
      h.asInstanceOf[HTMLElement].e.onclick {_ => console.log(b)}
    })
    list.addAll(a)
  }
  private val btn = SimpleView.b("add")
  private lazy val seleO = $[Select](id+"s")
  def xml: Elem = <div id={id}>
    {ioHtml.xml}
    {btn.xml}
   <div id={id+"s"}>{list.xml}</div>
  </div>

  def read: Iterable[A] =  list
  def +=(b : A): Node = {
    list += b
    val h  = b.newHtml
    seleO.appendChild(h)
    h.asInstanceOf[HTMLElement].e.onclick {_ => console.log(b)}
    h
  }
  def createEvent(): Unit = {
    btn.html.e.onclick{
      c =>
        += (ioHtml.toValue)
    }
  }

  def doFilter( filter : A => Boolean): Unit = {

    list.foreach(a => {
      if(filter(a)){
        a.html.asInstanceOf[HTMLElement].style.display = "block"
      }else{
        a.html.asInstanceOf[HTMLElement].style.display = "none"
      }
    })
  }
}
object Propose{

}
