package bon.jo.test
import bon.jo.html.HtmlEventDef._
import bon.jo.html.DomShell.{ExtendedHTMLCollection,ExtendedElement}
import bon.jo.memo.Dao.Id
import bon.jo.test.HTMLDef.{$l, $va_t}
import bon.jo.test.HtmlRep.ListRep
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{HTMLElement, Node}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}







class Propose[A :HtmlRep ,B <:raw.HTMLElement](list: mutable.ListBuffer[A]
                                                  , val ioHtml: IOHtml[B,A]
                                                  , save : A => Future[Option[A]],
                                                  sel : A => Unit
                                              )(implicit executionContext: ExecutionContext)  {


  val rep: HtmlRep[A] = implicitly[HtmlRep[A]]
  def addAll (a : IterableOnce[A]): mutable.ListBuffer[A] = {
    val l = a.iterator.toList

    l.foreach(b => {
      val h  = rep.html(b)
      h.show(false)
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
      val h =  a._2.asInstanceOf[HTMLElement]
      if(filter(a._1)){
        h.show(true)
      }else{
        h.show(false)
      }
    })
  }
}

