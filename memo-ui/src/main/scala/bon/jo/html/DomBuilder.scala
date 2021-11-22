package bon.jo.html

import bon.jo.html.HTMLDef.HtmlOps
import bon.jo.html.HTMLDef.$c
import org.scalajs.dom.document
import org.scalajs.dom.console
import org.scalajs.dom.raw.{Element, HTMLElement,Node}
import bon.jo.html.HtmlEventDef.ExH
import scala.language.dynamics
import org.scalajs.dom.raw.CSSStyleDeclaration
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.raw.HTMLOptionElement

object DomBuilder:
  

  trait HtmlDsl{
  
    type HtmlBuilder[A<: HTMLElement] = A?=> A


    object $ extends scala.Dynamic :
      def tNode(str : String) = document.createTextNode(str)
      def me:HtmlBuilder[HTMLElement] =
      summon
      def row:HtmlBuilder[HTMLElement] =
        doOnMe(_._class = "row")
      def col:HtmlBuilder[HTMLElement] =
        doOnMe(_._class = "col")
      def css(f :CSSStyleDeclaration=> Unit ):HtmlBuilder[HTMLElement]=
        doOnMe(m => f(m.style))
      def attr(attr : (String,String) *):HtmlBuilder[HTMLElement] =
        doOnMe(_.$attr(attr.toList))
    
      def text(str : String):HtmlBuilder[HTMLElement] =
        doOnMe(_.textContent = str)
      def _class(str : String):HtmlBuilder[HTMLElement] =
        doOnMe(_._class = str)
      def addClass(str : String):HtmlBuilder[HTMLElement] =
        doOnMe(m =>str.split("\\s+").foreach(m.classList.add))
      
      def apply(l : Node *):HtmlBuilder[HTMLElement] =
        l foreach (c => doOnMe(_.appendChild(c)))
        summon
      def childs[A <: Node](l : A *):HtmlBuilder[HTMLElement] =
        l foreach (c => doOnMe(_.appendChild(c)))
        summon

      def childs[A <: Node](customize : A => Unit,l : A *) : HtmlBuilder[HTMLElement] =
        l foreach customize 
        childs[A](l : _ * )
      def cols[A <: HTMLElement](l : A *) : HtmlBuilder[HTMLElement] =
        childs[HTMLElement](_._class="col",l : _ *)
      
      def click(l : => Unit) : HtmlBuilder[HTMLElement] =
        doOnMe(_.$click(_ => l))
        
      def  ->(f : HTMLElement => Unit) : HtmlBuilder[HTMLElement]  = doOnMe(f)
      def doOnMe(f : HTMLElement => Unit):HtmlBuilder[HTMLElement] = 
        val ret  : HTMLElement = summon
        f(ret)
        ret

      def applyDynamic(str : String)(f : HtmlBuilder[HTMLElement]):HTMLElement =
        given HTMLElement = $c.selectDynamic[HTMLElement](str)
        f



    object $t extends scala.Dynamic:
      def applyDynamic[T<: HTMLElement](str : String)(f : HtmlBuilder[T]):T =
        given T = $c.selectDynamic[T](str)
        f
      def  ->[T<: HTMLElement](f : T => Unit) : HtmlBuilder[T]  = doOnMe(f)
      def doOnMe[T<: HTMLElement](f : T => Unit):HtmlBuilder[T] = 
        val ret  : T = summon
        f(ret)
        ret
      def value[T <: (HTMLInputElement|HTMLOptionElement)](value : Any):HtmlBuilder[T] = 
        val ret  : T = summon
        ret match
          case a :HTMLInputElement => a.value = value.toString
          case a :HTMLOptionElement => a.value = value.toString
        ret
      def onchange[T <: HTMLElement](f : T => Unit):HtmlBuilder[T] = 
        val ret  : T = summon
        ret.$change{ _ => f(ret)}
        ret

      def me[T<: HTMLElement]:HtmlBuilder[T] =
        summon
      def row[T<: HTMLElement]:HtmlBuilder[T] =
        doOnMe(_._class = "row")
      def col[T<: HTMLElement]:HtmlBuilder[T] =
        doOnMe(_._class = "col")
      def css[T<: HTMLElement](f :CSSStyleDeclaration=> Unit ):HtmlBuilder[T]=
        doOnMe(m => f(m.style))
      def attr[T<: HTMLElement](attr : (String,String) *):HtmlBuilder[T] =
        doOnMe(_.$attr(attr.toList))
      def btnActive[T<: HTMLElement]:HtmlBuilder[T] =
        _class("btn btn-primary")
    
      def text[T<: HTMLElement](str : String):HtmlBuilder[T] =
        doOnMe(_.textContent = str)
      def _class[T<: HTMLElement](str : String):HtmlBuilder[T] =
        doOnMe(_._class = str)
      def addClass[T<: HTMLElement](str : String):HtmlBuilder[T] =
        doOnMe(m =>str.split("\\s+").foreach(m.classList.add))
      
      def apply[T<: HTMLElement](l : Node *):HtmlBuilder[T] =
        l foreach (c => doOnMe[T](_.appendChild(c)))
        summon
      def childs[T<: HTMLElement](l : Node *):HtmlBuilder[T] =
        l foreach (c => doOnMe[T](_.appendChild(c)))
        summon

      def childs[T<: HTMLElement,A <: Node](customize : A => Unit,l : A *) : HtmlBuilder[T] =
        l foreach customize 
        childs(l : _ *)
      def cols[T<: HTMLElement,A <: HTMLElement](l : A *) : HtmlBuilder[T] =
        childs[T,A](_._class="col",l : _ *)
      
      def click[T<: HTMLElement](l : => Unit) : HtmlBuilder[T] =
        doOnMe(_.$click(_ => l))
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


    

    

    
