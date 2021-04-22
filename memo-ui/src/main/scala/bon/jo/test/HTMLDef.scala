package bon.jo.test

import org.scalajs.dom.document
import org.scalajs.dom.raw.{DOMTokenList, HTMLCollection, HTMLElement, Node, Text}

import scala.language.dynamics

object HTMLDef {
  implicit class HtmlOps[T <: HTMLElement](t: T) {

    object $classSelect extends scala.Dynamic {

      def apply(clSel: String): HTMLCollection = t.getElementsByClassName(clSel)

      def selectDynamic(clSel: String): HTMLCollection = apply(clSel)
    }
    def _class: DOMTokenList = t.classList
    def _class_=(s: String): Unit = {
      s.split(" ").foreach(t.classList.add)
    }

    def $to[A <: HTMLElement]: A = t.asInstanceOf[A]

    def $list(htmlList: Iterable[HTMLElement]): T = {
      htmlList.foreach(t appendChild _)
      t
    }

    def $attr(keyValue: (Any, Any)*): T = {
      keyValue.foreach(e => {
        t.setAttribute(e._1.toString, e._2.toString)

      })
      t
    }

    def +=(childRen: Node): T = {
      t.appendChild(childRen).asInstanceOf[HTMLElement]
      t
    }

    def $textContent(str: String): T = {
      t.textContent = str
      t
    }

    def ++=(childRens: Node*): T = {
      childRens foreach +=
      t
    }


  }

  import scala.language.dynamics

  object $c extends scala.Dynamic {
    def selectDynamic[T <: HTMLElement](tagName: String): T = {
      val ret = document.createElement(tagName).asInstanceOf[T]

      ret
    }
  }

  object $ref extends scala.Dynamic {

    //def selectDynamic[T <: HTMLElement](tagName: String): T = document.createElement(tagName).asInstanceOf[T]

    def applyDynamic(tagName: String)(d: HTMLElement => Unit): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      d(ret)
      ret
    }
  }

  object $ref_t extends scala.Dynamic {

    //def selectDynamic[T <: HTMLElement](tagName: String): T = document.createElement(tagName).asInstanceOf[T]

    def applyDynamic[T <: HTMLElement](tagName: String)(d: T => Unit): T = {
      val ret = document.createElement(tagName).asInstanceOf[T]
      d(ret)
      ret
    }
  }

  object $t extends scala.Dynamic {
    def apply(str: String): Text = document.createTextNode(str)

    def applyDynamic(tagName: String)(textContent: String): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      ret.textContent = textContent
      ret
    }
  }

  object $va extends scala.Dynamic {
    def applyDynamic(tagName: String)(htmlL: Node*): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      htmlL.foreach(ret.appendChild)
      ret
    }

  }

  object $attr extends scala.Dynamic {
    def applyDynamic(tagName: String)(htmlL: (Any, Any)*): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      ret.$attr(htmlL: _ *)
      ret
    }

    object t extends scala.Dynamic {
      def applyDynamic[T <: HTMLElement](tagName: String)(htmlL: (Any, Any)*): T = {
        val ret = document.createElement(tagName).asInstanceOf[T]
        ret.$attr(htmlL: _ *)
        ret
      }
    }

  }

  object $va_t extends scala.Dynamic {
    def applyDynamic[T <: HTMLElement](tagName: String)(htmlL: Node*): T = {
      val ret = document.createElement(tagName).asInstanceOf[T]
      htmlL.foreach(ret.appendChild)
      ret
    }

  }

  object $l extends scala.Dynamic {
    def applyDynamic(tagName: String)(htmlL: Iterable[Node]): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      htmlL.foreach(ret.appendChild)
      ret
    }
  }

  object $l_t extends scala.Dynamic {
    def applyDynamic[T <: HTMLElement](tagName: String)(htmlL: Iterable[Node]): T = {
      val ret = document.createElement(tagName).asInstanceOf[T]
      htmlL.foreach(ret.appendChild)
      ret
    }
  }


}
