package bon.jo.app

import org.scalajs.dom.raw.HTMLElement

case class ChildParent(child: HTMLElement, parent: HTMLElement)

object ChildParent {
  trait Maker:
  //  def apply(a : )
    def createHtml(name: String, value: Any): (HTMLElement, HTMLElement)

    def apply(name: String, value: Any): (String, ChildParent) =
      val (ref, cont) = this createHtml(name, value)
      (name, ChildParent(ref, cont))

    def caracrAllContMap(map: Iterable[(String, Any)]): Iterable[(String, ChildParent)] =
      map.map {
        case (ref, cont) => this (ref, cont)
      }
}