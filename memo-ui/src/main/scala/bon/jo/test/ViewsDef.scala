package bon.jo.test


import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.HTMLDef.{$attr, $attrns, $c, $l, $ref, $refns, $t, $va, HtmlOps}
import bon.jo.test.HtmlRep._
import bon.jo.test.MemoLists.MemoListJS
import org.scalajs.dom.html.{Anchor, Button, Div, Input, Span}
import org.scalajs.dom.raw.{Element, HTMLElement}

import scala.:+
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  val kwClass = "kwClass"

  def apply(): ViewsDef = new ViewsDef

  def kwIO() = new IOHtml[Input, KeyWord]($c.input: Input, input => KeyWord(None, input.value))

  class ProposeInput[A](strF: A => String, textCreer: String)(
    list: mutable.ListBuffer[A]
    , override val ioHtml: IOHtml[Input, A]
    , save: A => Future[Option[A]],
    sel: A => Unit
  )(implicit executionContext: ExecutionContext, htmlRep: HtmlRep[A])
    extends Propose[A, Input](list, ioHtml, save, sel) {
    btn.textContent = textCreer
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

  val svgNs = "http://www.w3.org/2000/svg"

  def help: Element = {

    (
      $refns.svg(svgNs, { svg: Element =>
        svg.$attr("width" -> "16", "height" -> "16", "fill" -> "currentColor",
          "class" -> "bi bi-question-circle", "viewBox" -> "0 0 16 16") ++= (
          $attrns path(svgNs, "d" -> "M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z")
          , $attrns path(svgNs, "d" -> "M5.255 5.786a.237.237 0 0 0 .241.247h.825c.138 0 .248-.113.266-.25.09-.656.54-1.134 1.342-1.134.686 0 1.314.343 1.314 1.168 0 .635-.374.927-.965 1.371-.673.489-1.206 1.06-1.168 1.987l.003.217a.25.25 0 0 0 .25.246h.811a.25.25 0 0 0 .25-.25v-.105c0-.718.273-.927 1.01-1.486.609-.463 1.244-.977 1.244-2.056 0-1.511-1.276-2.241-2.673-2.241-1.267 0-2.655.59-2.75 2.286zm1.557 5.763c0 .533.425.927 1.01.927.609 0 1.028-.394 1.028-.927 0-.552-.42-.94-1.029-.94-.584 0-1.009.388-1.009.94z")
        )
      }))
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
      ret += ViewsDef.closeBtn


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





