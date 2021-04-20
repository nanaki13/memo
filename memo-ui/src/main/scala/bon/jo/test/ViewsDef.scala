package bon.jo.test


import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.HtmlRep._
import org.scalajs.dom.html.{Anchor, Div, Element}
import org.scalajs.dom.raw.{HTMLElement, Node, Text}
import org.scalajs.dom.{console, document}
import bon.jo.html.HtmlEventDef._
import bon.jo.test.HTMLDef.{$l, $ref, $t, $va, HtmlOps}
import bon.jo.test.MemoLists.MemoListJS

import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  def apply(): ViewsDef = new ViewsDef
}

class ViewsDef() {


  implicit val memoXml: XmlRepParam[Memo, MemoListView] = {
    (memo, mList) =>
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
                  mList.foreach(ml => {
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
  implicit val keyWord: HtmlRep[KeyWord] = HtmlRep {
    (memo) =>
      $t div {
        memo.value
      }

  }

  implicit val memoKeyWordXml: XmlRepParam[MemoKeywords, MemoListView] = {


    (memo, lisCpnt) => {
      var l: List[HTMLElement] = List(memo.memo.htmlp(lisCpnt),
        $t h3 "tags",$l div memo.keyWords.html
      )
      memo.memo.memoType match {
        case MemoType.Json => {
          l = l :+ ($ref button {
            save =>
              save.textContent = "save"
              save._class = "btn-save btn btn-primary"
          })
        }
        case MemoType.Text => l = l :+ ($ref button {
          edit =>
            edit.textContent = "edit"
            edit._class = "btn-edit btn btn-primary"
        })
      }
      $l div l
    }

  }
}





