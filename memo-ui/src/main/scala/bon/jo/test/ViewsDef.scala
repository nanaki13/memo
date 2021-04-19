package bon.jo.test


import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.XmlRep._
import org.scalajs.dom.html.{Anchor, Div, Element}
import org.scalajs.dom.raw.{HTMLElement, Node, Text}
import org.scalajs.dom.{console, document}
import bon.jo.html.HtmlEventDef._
import bon.jo.test.HTMLDef.{$t, $va, HtmlOps, $ref }

import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  def apply(): ViewsDef = new ViewsDef
}

class ViewsDef() {


  implicit val memoXml:  XmlRepParam[Memo,MemoListIpnutR] = {
    (memo,mList) =>
      $va div($ref h1 {
        _ :+ ($ref a {
          lienTilre =>
            lienTilre._class = "a-title"
            lienTilre.textContent = memo.title
            lienTilre.asInstanceOf[Anchor].href = s"/app/memo/${memo.id.getOrElse(0)}"
        })
      }
        , $ref div {
        tpeDiv =>
          tpeDiv._class = "m-type"
          tpeDiv :+ ($t div
            s"""Type : ${
              memo.memoType match {
                case MemoType.Text => "Text"
                case MemoType.Json => "List"
              }
            }""")

      }
        , $t h2 "Contenu",
        $ref div {
          cnt =>
            cnt._class = "m-content"

            memo.memoType match {
              case MemoType.Text => cnt.textContent = memo.content
              case MemoType.Json =>
                Try {
                  mList.foreach(ml =>{
                    ml.data = JSON.parse(memo.content).asInstanceOf[MemoListJS]

                    cnt :+ ml.html
                    ml.addEvent()
                  })

                  mList
                } match {
                  case Failure(a) => s"Erreur en traitant : ${memo.content}\n$a"
                  case Success(value) => value.toString()
                }
            }
        }
      )
  }
  implicit val keyWord: XmlRep[KeyWord] = {
    (memo,_) =>

      $t div {
        memo.value
      }

  }

  implicit val memoKeyWordXml: XmlRepParam[MemoKeywords,MemoListIpnutR] = {
    (memo,lisCpnt) =>
      $va div(
        memo.memo.htmlp(lisCpnt),
        $t h3 "tags"
        , $ref button {
        edit =>
          edit.textContent = "edit"
          edit._class = "btn-edit btn btn-primary"
      }, $ref button {
        save =>
          save.textContent = "save"
          save._class = "btn-save btn btn-primary"
      }
      )
  }


}

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
    def apply(str : String): Text = document.createTextNode(str)
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



