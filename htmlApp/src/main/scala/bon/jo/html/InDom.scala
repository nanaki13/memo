package bon.jo.html

import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.phy.{Obs, OnceObs}

import scala.xml.{Elem, Node}

trait InDom[Me <: HTMLElement] {
  _: IdView =>
  lazy val me: Me = DomShell.$[Me](id)


  def init(parent: HTMLElement)

  def isInDom: Boolean = DomShell.$[Me](id) match {
    case null => false
    case a: Any if !scalajs.js.isUndefined(a) => true
    case _ => false
  }

  def removeFromView(): Unit = {
    me.removeFromDom()
  }



}
object InDom{

  class simple[Me<: HTMLElement](node : Elem) extends  InDom[Me]() with IdView with XmlHtmlView[Me] {
    override def init(parent: HTMLElement): Unit = {
      parent.appendChild(html())
    }

    override def id: String = node \@ "id"

    override def xml(): Elem = node
  }
  class clk[Me<: HTMLElement](node : Elem) extends  simple[Me](node) with Clickable[Me]
  def apply[Me<: HTMLElement](node : Elem): InDom[Me] with XmlHtmlView[Me]= new simple(node)
  def clk[Me<: HTMLElement](node : Elem): InDom[Me]with XmlHtmlView[Me] = new clk(node)
}
trait Clickable[Me <: HTMLElement] extends InDom[Me] with IdView {
  val obs: OnceObs[MouseEvent] =  Obs.once[MouseEvent]()

  override def init(parent: HTMLElement): Unit = {
    me.addEventListener("click",obs.newValue)
  }
  def obsClick(): Obs[MouseEvent] = obs
}
