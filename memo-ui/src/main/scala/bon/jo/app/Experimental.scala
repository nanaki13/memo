package bon.jo.app

import bon.jo.html.HTMLDef.HtmlOps
import org.scalajs.dom.document
import org.scalajs.dom.raw.{Element, HTMLElement,Node}

import scala.language.dynamics

object Experimental {


  object tag extends scala.Dynamic {
    def selectDynamic(f: String): String = f
  }

  implicit class StringToHtml(s: String) {
    def toHtlm : HTMLElement = document.createElement(s).asInstanceOf[HTMLElement]
    def tag(tagHtml: String): HTMLElement = {
      document.createElement(tagHtml).asInstanceOf[HTMLElement] := (_.textContent = s)

    }

    def tagTyped[A <: Element](tagHtml: String): A = {
      document.createElement(tagHtml).asInstanceOf[A] := (_.textContent = s)

    }
  }

  implicit class ChildToParent(s: Node) {
    def wrap(tagHtml: String): HTMLElement = {
      document.createElement(tagHtml).asInstanceOf[HTMLElement] := (_ += s)
    }

    def wrap(tagHtml: HTMLElement): HTMLElement = {
      tagHtml.appendChild(tagHtml)
      tagHtml
    }
  }

  implicit class BsHtml[A <: HTMLElement](a: A) {
    def $row: A = {
      a.classList.add("row")
      a
    }

    def $row(col: HTMLElement*): A = {
      a.classList.add("row")
      col.map(_.$col).foreach(a += _)
      a
    }

    def $col: A = {
      a.classList.add("col")
      a
    }
  }
}
