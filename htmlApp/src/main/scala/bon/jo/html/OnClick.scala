package bon.jo.html

import bon.jo.html.DomShell.button
import org.scalajs.dom.html.Button
//
//trait OnClick[R <: Element] extends EventFromView[R] with InDom[R] with IdView {
//
//  override def init(p: HTMLElement): Unit = {
//
//    myEvent(id) match {
//      case None => throw new Exception("dont find " + id)
//      case Some(func) => me.addEventListener("click", func)
//    }
//  }
//
//
//}

trait OnClick extends LeaveView[Button] with InDom[Button]
object OnClick {
  type ButtonType = LeaveView[Button] with InDom[Button] with Clickable[Button]

  def apply(idp: String, label: String = ""): ButtonType = new LeaveView[Button] with InDom[Button] with Clickable[Button] {
    override def id: String = idp

    override def html(): Button = toElement(button(id = id, label))


  }
}