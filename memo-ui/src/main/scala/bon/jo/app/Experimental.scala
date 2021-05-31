package bon.jo.app

import bon.jo.html.HTMLDef.HtmlOps
import org.scalajs.dom.document
import org.scalajs.dom.console
import org.scalajs.dom.raw.{Element, HTMLElement,Node}
import bon.jo.html.HtmlEventDef.ExH
import scala.language.dynamics
import org.scalajs.dom.raw.CSSStyleDeclaration
import org.scalajs.dom.raw.HTMLElement

object Experimental:

  trait HtmlDsl extends scala.Dynamic{
    type HtmlBuilder = HTMLElement ?=> HTMLElement
    def applyDynamic(str : String)(f : HTMLElement ?=> HTMLElement):HTMLElement =
      given  HTMLElement = str.toHtlm
      f
    def  ->(f : HTMLElement => Unit) : HtmlBuilder  = doOnMe(f)
    def doOnMe(f : HTMLElement => Unit):HtmlBuilder = 
      val ret  : HTMLElement = summon
      f(ret)
      ret

    def me:HtmlBuilder =
      summon
    def row:HtmlBuilder =
      doOnMe(_._class = "row")
    def col:HtmlBuilder =
      doOnMe(_._class = "col")
    def css(f :CSSStyleDeclaration=> Unit ):HtmlBuilder=
      doOnMe(m => f(m.style))
    def attr(attr : (String,String) *):HtmlBuilder =
      doOnMe(_.$attr(attr.toList))
  
    def text(str : String):HtmlBuilder =
      doOnMe(_.textContent = str)
    def _class(str : String):HtmlBuilder =
      doOnMe(_._class = str)
    def addClass(str : String):HtmlBuilder =
      doOnMe(m =>str.split("\\s+").foreach(m.classList.add))
     
     def apply(l : Node *):HtmlBuilder =
      l foreach (c => doOnMe(_.appendChild(c)))
      summon
    def childs(l : Node *):HtmlBuilder =
      l foreach (c => doOnMe(_.appendChild(c)))
      summon

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


    

    

    
