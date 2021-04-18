package bon.jo.test


import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.XmlRep._
import org.scalajs.dom.html.{Anchor, Div}
import org.scalajs.dom.raw.{HTMLElement, Node}
import org.scalajs.dom.{console, document}
import bon.jo.html.HtmlEventDef._
import bon.jo.test.Test.{$t, $va, Tset, TestV => $}

import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  def apply(): ViewsDef = new ViewsDef
}

class ViewsDef() {

  import $._

  implicit val memoXml: XmlRepCapt[Memo, MemoListIpnutR] = {
    (memo, catpeur) =>
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
                  catpeur(Some(l))
                  l.xml
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

  implicit val memoKeyWordXml: XmlRepCapt[MemoKeywords, MemoListIpnutR] = {
    (memo, catpeur) =>
      $va div(
        memo.memo.html(catpeur),
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

object Test {
  implicit class Tset(t: HTMLElement) {
    def _class_=(s: String): Unit = {
      s.split(" ").foreach(t.classList.add)
    }

    def _class = t.classList
  }

  import scala.language.dynamics

  object TestV extends scala.Dynamic {

    //def selectDynamic[T <: HTMLElement](tagName: String): T = document.createElement(tagName).asInstanceOf[T]

    def applyDynamic(tagName: String)(d: HTMLElement => Unit): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      d(ret)
      ret
    }

    implicit class HtmlOps(html: HTMLElement) {
      def :+(childRen: HTMLElement): HTMLElement = html.appendChild(childRen).asInstanceOf[HTMLElement]
    }

  }

  object $t extends scala.Dynamic {
    def applyDynamic(tagName: String)(textContent: String): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      ret.textContent = textContent
      ret
    }
  }

  object $va extends scala.Dynamic {
    def applyDynamic(tagName: String)(htmlL: HTMLElement*): HTMLElement = {
      val ret = document.createElement(tagName).asInstanceOf[HTMLElement]
      htmlL.foreach(ret.appendChild)
      ret
    }
  }

  import bon.jo.test.Test.{TestV => $}
  import $._

  val htmlTest = $ div {
    root =>
      List($ button { btnAction =>
        btnAction.textContent = "Click MOi"
        btnAction._class = "btn btn-primary"
        btnAction.$click { _ => btnAction :+ ($ div (salutDiv => salutDiv.textContent = "Salut")) }
      }).foreach(root :+ _)
      root :+ (
        $ select {
          select =>

            List("A", "B", "C").foreach(letre => select :+ ($ option (o => o.textContent = letre)))
        }

        )

  }


}



