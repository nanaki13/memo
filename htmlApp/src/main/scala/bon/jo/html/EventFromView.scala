//package bon.jo.html
//
//import org.scalajs.dom.Event
//import org.scalajs.dom.raw.HTMLElement
//
//import scala.collection.mutable
//import scala.scalajs.js
//
//trait EventFromView[A <: HTMLElement ] {
//  self: InDom[A] with IdView =>
//  def eventsHadlers: mutable.Map[String, js.Function1[Event, _]] = EventFromView.eventsHadlers
//
//  def removeEvents(): Option[js.Function1[Event, _]] = eventsHadlers.remove(id)
//
//  def myEvent[R <: Event](value: String): Option[js.Function1[R, _]] = eventsHadlers.get(value)
//
//  def onClick(function: js.Function1[Event, _]): Unit = {
//    eventsHadlers.get(id) match {
//      case Some(value) => {
//        if (isInDom) {
//          self.me.removeEventListener("click", value)
//        }
//      }
//      case None =>
//    }
//    eventsHadlers(id) = function
//
//  }
//}
//
//object EventFromView {
//  val eventsHadlers: mutable.Map[String, js.Function1[Event, _]] = mutable.Map()
//}