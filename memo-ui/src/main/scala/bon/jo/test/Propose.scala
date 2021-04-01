package bon.jo.test
import bon.jo.html.DomShell.{$, $c}
import bon.jo.html.HtmlEventDef._
import bon.jo.memo.Dao.Id
import bon.jo.test
import bon.jo.test.XmlRep.{ListRep, _}
import org.scalajs.dom.html.{Button, Div, Select}
import org.scalajs.dom.raw.{HTMLElement, Node}
import org.scalajs.dom.{console, document, raw}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.Elem
trait HtmlExtract[A,B<: raw.Element]{
    def extract(html : B) : A
}


class IOHtml[A <: raw.Element,E](xmlf :String  => Elem,extractF: A => E) extends DomCpnt[A] with HtmlExtract[E,A] {
  override def xml: Elem = xmlf(id)

  override def extract(htmlp: A): E = extractF(htmlp)
  def toValue: E = extract(html)
}
object HtmlExtract{
  trait HtmlExtractAny[A] extends HtmlExtract[A, raw.Element]
  implicit class HtmlValue[A <: raw.Element,B : HtmlExtractAny](a : A){
      def toValue: B = implicitly[HtmlExtractAny[B]].extract(a)
  }
}


class Propose[A :XmlRep :Id,B <:raw.HTMLElement]( list: mutable.ListBuffer[A]
                                                ,val ioHtml: IOHtml[B,A]
                                              ,save : A => Future[Option[A]],
                                                sel : A => Unit
                                              )(implicit executionContext: ExecutionContext) extends DomCpnt[Div] {


  val idimp: Id[A] = implicitly[Id[A]].prefix(id)
  val rep: XmlRep[A] = implicitly[XmlRep[A]]
  def addAll (a : IterableOnce[A]): list.type = {
    val l = a.iterator.toList

    l.foreach(b => {
      val h  = b.newHtml(idimp,rep)
      h.asInstanceOf[HTMLElement].style.display = "none"
      seleO.appendChild(h)
      h.asInstanceOf[HTMLElement].e.onclick {_ => sel(b)}
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
    val h  = b.newHtml(idimp,rep)
    seleO.appendChild(h)
    h.asInstanceOf[HTMLElement].e.onclick {_ => sel(b)}
    h
  }
  def createEvent(): Unit = {
    btn.html.e.onclick{
      _ =>
        save(ioHtml.toValue).recover{
          case _ => None
        }.foreach{
          case None => PopUp("Marche pas...")
          case Some(value) => +=(value)
        }

    }
  }

  def doFilter( filter : A => Boolean): Unit = {

    list.foreach(a => {
      if(filter(a)){
        a.html(idimp).asInstanceOf[HTMLElement].style.display = "block"
      }else{
        a.html(idimp).asInstanceOf[HTMLElement].style.display = "none"
      }
    })
  }
}

