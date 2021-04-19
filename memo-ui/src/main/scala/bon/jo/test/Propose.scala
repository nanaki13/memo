package bon.jo.test
import bon.jo.html.HtmlEventDef._
import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.memo.Dao.Id
import bon.jo.test.HTMLDef.{$l, $va_t}
import bon.jo.test.XmlRep.ListRep
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{HTMLElement, Node}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
trait HtmlExtract[A,B<: raw.Element]{
    def extract(html : B) : A
}


class IOHtml[A <: raw.Element,E](xmlf :  => A,extractF: A => E) extends HtmlExtract[E,A] {
  val html: A =xmlf

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
                                              )(implicit executionContext: ExecutionContext)  {


 // val idimp: Id[A] = implicitly[Id[A]].prefix(id)
  val rep: XmlRep[A] = implicitly[XmlRep[A]]
  def addAll (a : IterableOnce[A]): mutable.ListBuffer[A] = {
    val l = a.iterator.toList

    l.foreach(b => {
      val h  = rep.html(b)
      h.style.display = "none"
      seleO.appendChild(h)
      h.$click {_ => sel(b)}
    })
    list.addAll(a)
  }
  private val btn = SimpleView.b("add")
  private val seleO = $l div list.html
  def html: Div = {
    $va_t div (ioHtml.html,btn,seleO)
  }

  def read: Iterable[A] =  list
  def +=(b : A): Node = {
    list += b
    val h  = rep.html(b)
    seleO.appendChild(h)
    h.$click {_ => sel(b)}
    h
  }
  def createEvent(): Unit = {
    btn.$click{
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

    (list zip seleO.children).foreach(a => {
      if(filter(a._1)){
        a._2.asInstanceOf[HTMLElement].style.display = "block"
      }else{
        a._2.asInstanceOf[HTMLElement].style.display = "none"
      }
    })
  }
}

