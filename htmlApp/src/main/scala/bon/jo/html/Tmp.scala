package bon.jo.html

import org.scalajs.dom.raw.{DragEvent, Event, FocusEvent, HTMLElement, KeyboardEvent, MouseEvent, PointerEvent, UIEvent, WheelEvent}

import scala.scalajs.js

class Tmp(html: HTMLElement) {
  object e {
    val onblur: (FocusEvent => Unit) => Unit = HtmlEventDef.blur(html, _)
    val onclick: (MouseEvent => Unit) => Unit = HtmlEventDef.click(html, _)
    val onkeyup: (KeyboardEvent => Unit) => Unit = HtmlEventDef.keyup(html, _)

    def onAction(doThis: => Unit): Unit = {
      onkeyup { e => if (e.keyCode == 13) doThis }
    }

    var onfocus: js.Function1[FocusEvent, _] = _
    var onmouseleave: js.Function1[MouseEvent, _] = _
    var onbeforecut: js.Function1[DragEvent, _] = _
    var onkeydown: js.Function1[KeyboardEvent, _] = _

    var onreset: js.Function1[Event, _] = _
    var onhelp: js.Function1[Event, _] = _
    var ondragleave: js.Function1[DragEvent, _] = _

    var onfocusin: js.Function1[FocusEvent, _] = _
    var onseeked: js.Function1[Event, _] = _


    var ondurationchange: js.Function1[Event, _] = _


    var onemptied: js.Function1[Event, _] = _
    var onseeking: js.Function1[Event, _] = _
    var oncanplay: js.Function1[Event, _] = _
    var ondeactivate: js.Function1[UIEvent, _] = _

    var onloadstart: js.Function1[Event, _] = _
    var ondragenter: js.Function1[DragEvent, _] = _
    var onsubmit: js.Function1[Event, _] = _

    var onchange: js.Function1[Event, _] = _

    var onbeforeactivate: js.Function1[UIEvent, _] = _
    var oncanplaythrough: js.Function1[Event, _] = _


    var onsuspend: js.Function1[Event, _] = _

    var onmouseenter: js.Function1[MouseEvent, _] = _

    var onmouseout: js.Function1[MouseEvent, _] = _

    var onmousewheel: js.Function1[WheelEvent, _] = _
    var onvolumechange: js.Function1[Event, _] = _


    /**
     * The ParentNode.children read-only property returns a live HTMLCollection of
     * child elements of the given object.
     *
     * The items in the returned collection are objects and not strings. To get data
     * from those node objects, you must use their properties (e.g.
     * elementNodeReference.children[1].nodeName to get the name, etc.).
     *
     * MDN
     */
    var ondragend: js.Function1[DragEvent, _] = _
    var onbeforepaste: js.Function1[DragEvent, _] = _
    var ondragover: js.Function1[DragEvent, _] = _


    var onmouseup: js.Function1[MouseEvent, _] = _
    var ondragstart: js.Function1[DragEvent, _] = _
    var onbeforecopy: js.Function1[DragEvent, _] = _
    var ondrag: js.Function1[DragEvent, _] = _
    var onmouseover: js.Function1[MouseEvent, _] = _


    var onpause: js.Function1[Event, _] = _

    var onmousedown: js.Function1[MouseEvent, _] = _

    var onwaiting: js.Function1[Event, _] = _


    var onstalled: js.Function1[Event, _] = _
    var onmousemove: js.Function1[MouseEvent, _] = _


    var onratechange: js.Function1[Event, _] = _


    var contentEditable: String = _


    var onprogress: js.Function1[js.Any, _] = _
    var ondblclick: js.Function1[MouseEvent, _] = _
    var oncontextmenu: js.Function1[MouseEvent, _] = _
    var onloadedmetadata: js.Function1[Event, _] = _
    var onplay: js.Function1[Event, _] = _
    var onplaying: js.Function1[Event, _] = _

    var onfocusout: js.Function1[FocusEvent, _] = _
    var onabort: js.Function1[UIEvent, _] = _

    var onreadystatechange: js.Function1[Event, _] = _
    var onkeypress: js.Function1[KeyboardEvent, _] = _
    var onloadeddata: js.Function1[Event, _] = _
    var onbeforedeactivate: js.Function1[UIEvent, _] = _

    var onactivate: js.Function1[UIEvent, _] = _
    var onselectstart: js.Function1[Event, _] = _
    var ontimeupdate: js.Function1[Event, _] = _
    var onselect: js.Function1[UIEvent, _] = _
    var ondrop: js.Function1[DragEvent, _] = _


    var onended: js.Function1[Event, _] = _
    var onscroll: js.Function1[UIEvent, _] = _

    var oninput: js.Function1[Event, _] = _


    var oncuechange: js.Function1[Event, _] = _


    var onpointerover: js.Function1[PointerEvent, _] = _

    var onpointerenter: js.Function1[PointerEvent, _] = _


    var onpointerdown: js.Function1[PointerEvent, _] = _

    var onpointermove: js.Function1[PointerEvent, _] = _

    var onpointerup: js.Function1[PointerEvent, _] = _


    var onpointercancel: js.Function1[PointerEvent, _] = _

    var onpointerout: js.Function1[PointerEvent, _] = _

    var onpointerleave: js.Function1[PointerEvent, _] = _


    var gotpointercapture: js.Function1[PointerEvent, _] = _

    var lostpointercapture: js.Function1[PointerEvent, _] = _
  }

}
