package bon.jo.test

import org.scalajs.dom.document
import org.scalajs.dom.raw.{HTMLElement, Node, Text}

object HTMLDef {
  implicit class HtmlOps(t: HTMLElement) {
    def _class_=(s: String): Unit = {
      s.split(" ").foreach(t.classList.add)
    }

    def $to[T <: HTMLElement]: T = t.asInstanceOf[T]

    def $list(htmlList: Iterable[HTMLElement]): HTMLElement = {
      htmlList.foreach(t appendChild _)
      t
    }

    def $attr(keyValue: (String, String)*): HTMLElement = {
      keyValue.foreach(e => {
        t.setAttribute(e._1, e._2)

      })
      t
    }

    def :+(childRen: HTMLElement): HTMLElement = t.appendChild(childRen).asInstanceOf[HTMLElement]

    def :++(childRens: HTMLElement*): Unit = childRens foreach :+

    def _class = t.classList
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

  object $va_t extends scala.Dynamic {
    def applyDynamic[T <: HTMLElement](tagName: String)(htmlL: HTMLElement*): T = {
      val ret = document.createElement(tagName).asInstanceOf[T]
      htmlL.foreach(ret.appendChild)
      ret
    }

  }

  object $l extends scala.Dynamic {
    def applyDynamic(tagName: String)(htmlL: Iterable[HTMLElement]): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      htmlL.foreach(ret.appendChild)
      ret
    }
  }

  object $l_t extends scala.Dynamic {
    def applyDynamic[T <: HTMLElement](tagName: String)(htmlL: Iterable[HTMLElement]): T = {
      val ret = document.createElement(tagName).asInstanceOf[T]
      htmlL.foreach(ret.appendChild)
      ret
    }
  }


}
