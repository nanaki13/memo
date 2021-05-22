package bon.jo.app

import bon.jo.html.HTMLDef.HtmlOps
import org.scalajs.dom.document
import org.scalajs.dom.console
import org.scalajs.dom.raw.{Element, HTMLElement,Node}
import bon.jo.html.HtmlEventDef.ExH
import scala.language.dynamics
import org.scalajs.dom.raw.CSSStyleDeclaration
import org.scalajs.dom.raw.HTMLHtmlElement

object Experimental:

  trait HtmlDsl extends scala.Dynamic{
    def applyDynamic(str : String)(f : HTMLElement ?=> HTMLElement):HTMLElement =
      given  HTMLElement = str.toHtlm
      f


    def me(using v : HTMLElement) : HTMLElement =
      v
    def row(using v : HTMLElement) : HTMLElement =
      v._class = "row"
      v
    def col(using v : HTMLElement) : HTMLElement =
      v._class = "col"
      v
    def css(f :CSSStyleDeclaration=> Unit )(using v : HTMLElement) : HTMLElement =
      f(v.style)
      v
    def text(str : String)(using v : HTMLElement) : HTMLElement =
      v.textContent = str
      v
    def _class(str : String)(using v : HTMLElement) : HTMLElement =
      v._class = str
      v
    def childs(l : Node *)(using v : HTMLElement) : HTMLElement =
      l foreach v.appendChild
      v

    def childs[A <: Node](customize : A => Unit,l : A *)(using v : HTMLElement) : HTMLElement =
      l foreach customize 
      l foreach v.appendChild
      v
    def cols[A <: HTMLElement](l : A *)(using v : HTMLElement) : HTMLElement =
      childs[A](_._class="col",l : _ *)
    
    def click(l : => Unit)(using v : HTMLElement) : HTMLElement =
      v.$click(_ => l)
      v
  }


  object html extends HtmlDsl

 
  object tag extends scala.Dynamic:
    def selectDynamic(f: String): String = f

  extension (s: String)
    /**
     * @return an html with this tag
     * 
     * */
    def toHtlm : HTMLElement = document.createElement(s).asInstanceOf[HTMLElement]
    def tag(tagHtml: String): HTMLElement =
      document.createElement(tagHtml).asInstanceOf[HTMLElement] := (_.textContent = s)


    def tagTyped[A <: Element](tagHtml: String): A =
      document.createElement(tagHtml).asInstanceOf[A] := (_.textContent = s)


  extension (s: Node)
    def wrap(tagHtml: String): HTMLElement =
      document.createElement(tagHtml).asInstanceOf[HTMLElement] := (_ += s)

    def wrap(tagHtml: HTMLElement): HTMLElement =
      tagHtml.appendChild(tagHtml)
      tagHtml

  extension [A <: HTMLElement](a: A)
    def $row: A =
      a.classList.add("row")
      a

    def $row(col: HTMLElement*): A =
      a.classList.add("row")
      col.map(_.$col).foreach(a += _)
      a

    def $col: A =
      a.classList.add("col")
      a


    

    

    
