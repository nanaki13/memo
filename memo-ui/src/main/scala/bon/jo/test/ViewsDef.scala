package bon.jo.test


import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.XmlRep._
import org.scalajs.dom.html.{Anchor, Div, Element}
import org.scalajs.dom.raw.{HTMLElement, Node, Text}
import org.scalajs.dom.{console, document}
import bon.jo.html.HtmlEventDef._
import bon.jo.test.HTMLDef.{$t, $va, HtmlOps, $ref => $}

import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  def apply(): ViewsDef = new ViewsDef
}

class ViewsDef() {

  import $._

  implicit val memoXml: XmlRep[Memo] = {
    memo =>
      $va div($ h1 {
        _ :+ ($ a {
          lienTilre =>
            lienTilre._class = "a-title"
            lienTilre.asInstanceOf[Anchor].href = s"/app/memo/${memo.id.getOrElse(0)}"
        })
      }
        , $ div {
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
        $ div {
          cnt =>
            cnt._class = "m-content"
            cnt.textContent = memo.memoType match {
              case MemoType.Text => memo.content
              case MemoType.Json =>
                Try {
                  val l = new MemoListIpnutR(JSON.parse(memo.content).asInstanceOf[MemoListJS])
                  l.addEvent()
                  l
                } match {
                  case Failure(_) => s"Erreur en traitant : ${memo.content}"
                  case Success(value) => value.toString()
                }
            }
        }
      )
  }
  implicit val keyWord: XmlRep[KeyWord] = {
    memo =>

      $t div {
        memo.value
      }

  }

  implicit val memoKeyWordXml: XmlRep[MemoKeywords] = {
    (memo) =>
      $va div(
        memo.memo.html,
        $t h3 "tags"
        , $ button {
        edit =>
          edit._class = "btn-edit btn btn-primary"
      }, $ button {
        save =>
          save._class = "btn-edit btn btn-primary"
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
    def applyDynamic(tagName: String)(htmlL: Element*): HTMLElement = {
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


  val htmlTest = $ref div {
    root =>
      List($ref button { btnAction =>
        btnAction.textContent = "Click MOi"
        btnAction._class = "btn btn-primary"
        btnAction.$click { _ => btnAction :+ ($ref div (salutDiv => salutDiv.textContent = "Salut")) }
      }).foreach(root :+ _)
      root :+ (
        $ref select {
          select =>

            List("A", "B", "C").foreach(letre => select :+ ($ref option (o => o.textContent = letre)))
        }

        )

  }


}



