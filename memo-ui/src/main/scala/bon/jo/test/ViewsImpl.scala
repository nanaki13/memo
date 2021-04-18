package bon.jo.test

import bon.jo.html.{AutoId, GenId}
import bon.jo.html.DomShell.{$, ExtendedElement, ExtendedHTMLCollection, ExtendedNode}
import bon.jo.html.HtmlEventDef._
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords, MemoType}
import bon.jo.test.SimpleView.{dcselect, i, s, ta}
import bon.jo.test.HTMLDef._
import bon.jo.test.XmlRep._
import org.scalajs.dom.{console, raw}
import org.scalajs.dom.experimental.URLSearchParams
import org.scalajs.dom.html.{Div, Element, Input, Select, Span, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLUListElement}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

object MemoListIpnutR {


  def apply() = new MemoListIpnutR
}

class MemoListIpnutR(val data: MemoListJS = new MemoList(Nil.toJSArray)) extends GenId {
  val tInput: Input = $c.input

  HTMLDef
  implicit val idEl: Id[ListElementJS] = me => s"$id-${me.content}"


  val ckeckClass = "input-li"
  val spanWordClass = "span-li"
  val deleteClass = "delete-li"
  implicit val listElementidXmlRep: XmlRep[ListElementJS] = {
    li =>
      $va li( {
        val inp: Input = $c.input
        if (li.checked) {
          inp.checked = true
        }

        inp.`type` = "checkbox"
        inp

      }, $ref span { sContent =>
        sContent._class = spanWordClass
        sContent.textContent = li.content
      })
  }
  implicit val idXmlRep: XmlRep[MemoListJS] = XmlRepImpl[MemoListJS](m => $ref ul {
    lUl =>
      lUl.id = id + "l"
      m.elements.toList.html.foreach(lUl :+ _)
  })


  lazy val list: HTMLUListElement = $[HTMLUListElement](id + "l")

  def html: Div = ($ref div {d =>
    d.id=id
    d._class ="form-group"
    d :++ (
      $ref label {
      l =>
          l.$attr(("for",tInput.id))
      },tInput,data.html
    )
  }).$to


  def elOfClass(htmlkElement: HTMLElement)(str: String): Iterable[Element] = htmlkElement.getElementsByClassName(str).map(_.asInstanceOf[HTMLElement])

  def del(htmlkElement: HTMLElement): Unit = elOfClass(htmlkElement)(deleteClass).foreach(d => d.$click { _ => htmlkElement.html.removeFromDom() })

  def addEvent(): Unit = {

    console.log(tInput)
    val ev = tInput
    ev.$Action {
      val el: ListElementJS = new ListElement(tInput.value, false)
      val htmlN = el.newHtml
      list.appendChild(htmlN)

      del(htmlN)
      tInput.value = ""
    }
    ev.$keyup {
      _ =>
        list.children.map(_.asInstanceOf[HTMLElement]).foreach { e =>
          val lElemntText = readWord(e)


          val show = lElemntText.toLowerCase.contains(tInput.value.toLowerCase)

          e.show(show)
        }
    }
    list.children.map(_.asInstanceOf[HTMLElement]).foreach(del)

  }

  private def readWord(l: raw.HTMLElement) = {
    elOfClass(l)(spanWordClass).head.innerText.trim
  }

  private def readCheck(l: raw.HTMLElement) = {
    elOfClass(l)(ckeckClass).map(_.asInstanceOf[Input]).head.checked
  }

  def read(): MemoList = {
    new MemoList(list.children.map(_.asInstanceOf[HTMLElement]).map(l => new ListElement(readWord(l), readCheck(l))).map(_.asInstanceOf[ListElementJS]).toJSArray)
  }
}

class MemoCtxView {


  val memoList: MemoListIpnutR = MemoListIpnutR()

  val tInput: Input = i
  val contentInput: TextArea = ta
  val memoType: Select = s[MemoType]


  def newMemo: Entities.Memo = {
    val mt = MemoType(memoType.value)
    val ret = mt match {
      case MemoType.Text => new Entities.Memo(tInput.value, contentInput.value, MemoType(memoType.value))
      case MemoType.Json => new Entities.Memo(tInput.value, JSON.stringify(memoList.read().pure()), MemoType(memoType.value))
    }

    ret

  }

  def makeSwitchView(): Unit = {
    memoType.html.onchange = { _ => {
      MemoType(memoType.value) match {
        case MemoType.Text =>
          contentInput.html.style.display = "block"
          memoList.html.style.display = "none"

        case MemoType.Json =>
          memoList.html.style.display = "block"
          contentInput.html.style.display = "none"
      }
    }

    }
  }
}

class ViewsImpl(implicit executionContext: ExecutionContext) {
  implicit val idMemo: Id[Entities.Memo] = m => "m" + m.id.getOrElse(m.title)
  implicit val idK: Id[KeyWord] = m => "k" + m.id.getOrElse(m.value)
  implicit val idMemoKw: Id[Entities.MemoKeywords] = m => "mk" + m.memo.id.getOrElse(m.memo.title)

  val viewsDef: ViewsDef = ViewsDef()

  import viewsDef.memoXml
  import viewsDef.keyWord

  object mCtx extends MemoCtxView

  //  object memoView extends SimpleView[Entities.Memo](() => <div>titre :
  //    {mCtx.tInput.xml}
  //    contenu :
  //    {mCtx.contentInput.xml}
  //  </div>) {
  //    def fillFromService: Future[Unit] = Daos.memoDao.readAll()
  //      .map(m => {
  //        m.foreach(+=)
  //      })
  //  }

  val keywWordI: Input = i

  object keyWordView extends SimpleView[Entities.KeyWord](() => $va div ( $va span(keyWord.html,$t(":")
    ,keywWordI))) {
    def fillFromService: Future[Iterable[KeyWord]] = Daos.keyWordDao.readAll()
      .map(m => {
        m.foreach(+=)
        m
      })
  }

  //  def addMemoLisner: Unit =
  //    memoView.btnInput.html.onclick = _ => {
  //      val m = mCtx.newMemo
  //      val req: Future[Unit] = Daos.memoDao.create(m).map(o => o.foreach(memoView.+=))
  //      req.onComplete {
  //        case Failure(exception) => throw (exception)
  //        case Success(_) =>
  //      }
  //
  //
  //    }





class MemoKeyWordViewListCreate(val propose: Propose[KeyWord, Input], listView: Div, val memoKeywWordtx: MemoCtxView)
                               (implicit idXmlRep: XmlRep[MemoKeywords], idM: Id[MemoKeywords], executionContext: ExecutionContext)
  extends SimpleView[Entities.MemoKeywords](() =>
    $va div(
      $t span "titre",
      memoKeywWordtx.tInput,
      $t span "type :",
      memoKeywWordtx.memoType,
      $t div "content",
      $va div(memoKeywWordtx.contentInput,memoKeywWordtx.memoList.html)
    )
    <div>titre
      {memoKeywWordtx.tInput}<div>type :
      {memoKeywWordtx.memoType.xml}
    </div>
      <div>Content :</div>
      <div>
        {memoKeywWordtx.contentInput.xml}{memoKeywWordtx.memoList.html}
      </div>{listView.xml}<div>KeyWord :</div>{propose.xml}
    </div>) {


  val searchParams = new URLSearchParams(org.scalajs.dom.window.location.search)
  val (limit, offset) = Try {
    (Option(searchParams.get("limit")).map(_.toInt).getOrElse(-1)
      , Option(searchParams.get("from")).map(_.toInt).getOrElse(-1))
  } match {
    case Failure(_) => (-1, -1)
    case Success(value) => value
  }

  def fillFromService: Future[Iterable[MemoKeywords]] = Daos.memoKeyWord.readAll(limit = limit, offset = offset)
    .map(m => {
      m.foreach(+=)
      m
    })

  def addMKw(iterable: Iterable[KeyWord]): Unit =


    btnInput.html.$click {
      _ => {
        val m = Entities.MemoKeywords(memoKeywWordtx.newMemo, iterable.toSet)
        val req: Future[Unit] = Daos.memoKeyWord.create(m).map(o => o.foreach(+=))
        req.onComplete {
          case Failure(exception) => throw (exception)
          case Success(_) =>
        }
      }
    }
}
