package bon.jo.test


import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.HTMLDef.{$attr, $c, $l, $ref, $t, $va, HtmlOps}
import bon.jo.test.HtmlRep._
import bon.jo.test.MemoLists.MemoListJS
import org.scalajs.dom.html.{Anchor, Div, Input, Span, Button}
import org.scalajs.dom.raw.HTMLElement


import scala.:+
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  val kwClass = "kwClass"

  def apply(): ViewsDef = new ViewsDef

  def kwIO() = new IOHtml[Input, KeyWord]($c.input: Input, input => KeyWord(None, input.value))

  class ProposeInput[A](strF: A => String,textCreer : String)(
    list: mutable.ListBuffer[A]
    , override val ioHtml: IOHtml[Input, A]
    , save: A => Future[Option[A]],
    sel: A => Unit
  )(implicit executionContext: ExecutionContext, htmlRep: HtmlRep[A])
    extends Propose[A, Input](list, ioHtml, save, sel) {
    btn.textContent=textCreer
    ioHtml.html.$keyup {
      v =>
        doFilter(ioHtml.html.value.trim.nonEmpty && strF(_).toLowerCase.contains(ioHtml.html.value.toLowerCase))
    }
  }

  val closeClass = "closeClass"
  def closeBtn: Span = {
    (($attr span("type" -> "button", "class" -> s"badge badge-secondary $closeClass", "aria-label" -> "Close")) +=
      $t("×")
      ).$to
  }
}

class ViewsDef() {


  implicit val memoXml: XmlRepParam[Memo, MemoListView] = {
    (memo, mList) =>
      $va div($ref h1 {
        _ += ($ref a {
          lienTilre =>
            lienTilre._class = "a-title"
            lienTilre.textContent = memo.title
            lienTilre.asInstanceOf[Anchor].href = s"/app/memo/${memo.id.getOrElse(0)}"
        })
      }
        , $ref div {
        tpeDiv =>
          tpeDiv._class = "m-type"
          tpeDiv += ($t div
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

                    cnt += ml.html
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
    (kw) =>
      val ret = $t span {
        kw.value
      }
      ret._class = s"badge badge-primary ${ViewsDef.kwClass}"
      ret +=  ViewsDef.closeBtn


  }



  class MKCpnt(memo: MemoKeywords, lisCpnt: MemoListView) {

    val kwDiv: Div = $l.t div memo.keyWords.html
    var l: List[HTMLElement] = List(memo.memo.htmlp(Some(lisCpnt)),
      $t h3 "tags", kwDiv
    )
    memo.memo.memoType match {
      case MemoType.Json =>
        l = l :+ ($ref button {
          save =>
            save.textContent = "save"
            save._class = "btn-save btn btn-primary"
        })
      case MemoType.Text => l = l :+ ($ref button {
        edit =>
          edit.textContent = "edit"
          edit._class = "btn-edit btn btn-primary"
      })
    }
    val html: Div = $l.t div l


  }

  implicit val memoKeyWordXml: XmlRepParam[MemoKeywords, MemoListView] = {


    (memo, lisCpnt) => {
      var l: List[HTMLElement] = List(memo.memo.htmlp(lisCpnt),
        $t h3 "tags", $l div memo.keyWords.html
      )
      memo.memo.memoType match {
        case MemoType.Json =>
          l = l :+ ($ref button {
            save =>
              save.textContent = "save"
              save._class = "btn-save btn btn-primary"
          })
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





