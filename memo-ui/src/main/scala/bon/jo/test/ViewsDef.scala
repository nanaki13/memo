package bon.jo.test


import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import HTMLDef.{$attr, $attrns, $c, $l, $ref, $refns, $t, $va, Ev, HtmlOps}
import bon.jo.test.HtmlRep._
import bon.jo.test.HtmlRep.HtmlCpnt._
import bon.jo.test.MemoLists.MemoListJS
import org.scalajs.dom.console
import org.scalajs.dom.html.{Anchor, Button, Div, Input, Span}
import org.scalajs.dom.raw.{Element, HTMLElement}
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.test.ViewsDef.ProposeInput
import bon.jo.test.SimpleView.DSelect
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  val kwClass = "kwClass"

  def apply(): ViewsDef = new ViewsDef

  def kwIO() = new IOHtml[Input, KeyWord]($c.input: Input, input => KeyWord(None, input.value))

  var events: Option[Ev] = None

  class ProposeInput[A](strF: A => String, textCreer: String)(

    override val ioHtml: IOHtml[Input, A]
    , save: A => Future[Option[A]],
    proposeView: ProposeView[A], sel: A => Unit
  )(implicit executionContext: ExecutionContext, htmlRep: HtmlRep[A])
    extends Propose[A, Input](ioHtml, save, proposeView) {
    btn.textContent = textCreer
    ioHtml.html.$keyup {
      v =>
        focus()
        proposeView.doFilter(ioHtml.html.value.trim.nonEmpty && strF(_).toLowerCase.contains(ioHtml.html.value.toLowerCase))
        events.foreach {
          e =>
            e.foreach { rlrv =>
              rlrv._1.removeEventListener("click", rlrv._2)
            }
        }
        events = Some(createAllEvent(sel))

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


  class MemoCpnt(val memo: Memo, val mList: Option[MemoListView]) extends HtmlCpnt with TypedHtml[Memo] {
    def lienTilre_(memo: Memo) = $ref a {
      lienTilre =>
        lienTilre._class = "a-title"
        lienTilre.textContent = memo.title
        lienTilre.asInstanceOf[Anchor].href = s"/app/memo/${memo.id.getOrElse(0)}"
    }

    val lienTilre = lienTilre_(memo)

    def content_(memol: Memo): HTMLElement = {
      $ref div {
        cnt =>
          cnt._class = "m-content"

          memol.memoType match {
            case MemoType.Text => cnt.innerHTML = memol.content.replaceAll("""#\[([^|]*)\|([^\s]*)]""", """<a href="$2">$1</a>""")
            case MemoType.Json =>
              Try {
                mList.foreach(ml => {
                  ml.data = JSON.parse(memol.content).asInstanceOf[MemoListJS]

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
    }

    val content = content_(memo)

    def textType(memop: Memo) =
      s"""Type : ${
        memop.memoType match {
          case MemoType.Text => "Text"
          case MemoType.Json => "List"
        }
      }"""

    val _tpeDiv = ($t div
      textType(memo))

    val body = $ref div {
      bdy =>
        bdy._class = "card-body"
        bdy ++= (
          $ref div {
            tpeDiv =>
              tpeDiv._class = "m-type"
              tpeDiv += _tpeDiv

          },
          content
        )
    }
    val html = List($ref div { h1Title =>
      h1Title += (lienTilre)
      h1Title._class = "card-title"
    }
      , body)

    override def get: IterableOnce[HTMLElement] = html


    override def update(value: Option[Memo]): Unit = {
      import HTMLDef._
      value foreach {
        m =>

          lienTilre.textContent = m.title
          _tpeDiv.innerText = textType(m)
          content.innerHTML = ""
          val c = content_(m)
          console.log(m.content)

          console.log(c.childNodes)
         // content.innerHTML = c.innerHTML
          c.childNodes.foreach {
            p => if(!scalajs.js.isUndefined(p)) {
              console.log(p)
              content.appendChild(p)
            }

          }
      }
    }
  }

  implicit val memoXml: HtmlRepParam[Memo, MemoListView] = {
    (memo, mList) => new MemoCpnt(memo, mList)
  }

  object KewWordHtml {
    object WithClose {
      implicit val keyWordWithClose: HtmlRep[KeyWord] = HtmlRep {
        (kw) =>

          val ret = keyWordWith.html(kw)
          val html = ret.get
          html.iterator.toList.head += ViewsDef.closeBtn
          console.log(ret.head)
          ret

      }
    }

    implicit val keyWordWith: HtmlRep[KeyWord] = HtmlRep {
      (kw) =>
        val ret = $t span {
          kw.value
        }
        ret._class = s"badge badge-primary m-1 ${ViewsDef.kwClass}"
        (() => (ret)).toHtmlCpnt

    }
  }

  implicit val cv: HtmlCpnt => MemoCpnt = _.asInstanceOf[MemoCpnt]

  class MKCpnt(memoInitial: MemoKeywords, proposeView: ProposeView[KeyWord])(implicit executionContext: ExecutionContext) extends HtmlCpnt with TypedHtml[MemoKeywords] {

    private var currentMemo = memoInitial

    val ctx = new MemoCtxView
    ctx.memoType.select(currentMemo.memo.memoType.toString)

    import KewWordHtml.WithClose._

    val kwDiv: Div = $l.t div memoInitial.keyWords.html.flatMap(_.list)
    console.log(kwDiv)
    val saveButton: Button = $ref.t button {
      save: Button =>
        save.textContent = "save"
        save._class = "btn-save btn btn-primary"
    }
    val editButton: Button = $ref.t button {
      save: Button =>
        save.textContent = "edit"
        save._class = "edit btn btn-primary"
    }
    val propose = new ProposeInput[KeyWord](_.value, "Creer/Chercher tags")(
      ViewsDef.kwIO(), Daos.keyWordDao.create, proposeView, addKeyWord)
    val footer: HTMLElement = $ref div {
      ff =>
        ff._class = "card-footer"
        ff ++= ($t h3 "tags", kwDiv, propose.html)
    }
    val cpnt = memoInitial.memo.typedHtml(Some(ctx.memoList))
    cpnt.body ++= (saveButton, editButton)
    val l: List[HTMLElement] = cpnt.list :+ footer
    implicit val keyWordsBuffer: ListBuffer[KeyWord] = ListBuffer.from(memoInitial.keyWords.toList)


    override def get: List[HTMLElement] = l


    def addKeyWord(selected: KeyWord): Unit = {
      keyWordsBuffer += selected
      val htmlKW = selected.html.list
      kwDiv ++= htmlKW
      deleteEvent(selected, htmlKW.head)

    }

    def deleteEvent(kw: KeyWord, htmlKw: HTMLElement)(implicit lubber: ListBuffer[KeyWord]): Unit = {

      htmlKw.$classSelect(ViewsDef.closeClass).foreach { btnClose =>
        btnClose.asInstanceOf[HTMLElement].$click {
          _ =>
            lubber -= kw
            htmlKw.removeFromDom()
        }
      }


    }

    override def update(value: Option[MemoKeywords]): Unit = {
      cpnt.update(value.map(_.memo))
    }

    def displayButton(): Unit = {
      currentMemo.memo.memoType match {
        case MemoType.Text => editButton.show(true)
        case MemoType.Json => editButton.show(true)
      }
    }

    def save() = {
      currentMemo = currentMemo.copy(memo = ctx.newMemo.copy(id = currentMemo.memo.id), keyWords = keyWordsBuffer.toSet)
      Daos.memoKeyWord.update(currentMemo).onComplete {
        case Failure(exception) => console.log(exception); PopUp("Sauvegarde KO")
        case Success(value) => PopUp("Sauvegarde OK")
          //  ctx.memoList.html.safeRm()
          ctx.memoType.safeRm()
          ctx.tInput.safeRm()
          ctx.contentInput.safeRm()
          update(value)
      }
      displayButton()
    }

    keyWordsBuffer zip kwDiv.$classSelect(ViewsDef.kwClass).map(_.asInstanceOf[HTMLElement]) foreach {
      deleteEvent _ tupled _
    }


    saveButton.$click { _ =>
      save()
    }

    editButton.$click { _ =>
      l.foreach(_.$classSelect.`a-title`.map(_.asInstanceOf[HTMLElement]).foreach { a =>
        a.parentElement += ctx.tInput
        ctx.tInput.value = currentMemo.memo.title
      })
      l.foreach(_.$classSelect.`m-content`.map(_.asInstanceOf[HTMLElement]).foreach { a =>

        a.parentElement.insertBefore(ctx.contentInput, saveButton)
        a.parentElement.insertBefore(ctx.memoList.html, saveButton)
        currentMemo.memo.memoType match {
          case MemoType.Text =>
            ctx.contentInput.value = currentMemo.memo.content
            ctx.contentInput.show(true)
            ctx.memoList.html.show(false)
          case MemoType.Json =>
        }

        ()
      })
      l.foreach(_.$classSelect.`m-type`.map(_.asInstanceOf[HTMLElement]).foreach { a =>
        a += (ctx.memoType)
        ctx.memoType.select(currentMemo.memo.memoType.toString)
      })
      ctx.makeSwitchView()
      ctx.memoList.addEvent()
    }
    displayButton()


  }


}





