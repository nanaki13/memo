package bon.jo.test

import bon.jo.html.{AutoId, GenId}
import bon.jo.html.DomShell.{$, ExtendedElement, ExtendedHTMLCollection, ExtendedNode}
import bon.jo.html.HtmlEventDef._
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords, MemoType}
import bon.jo.test.SimpleView.{i, s, ta}
import bon.jo.test.HTMLDef._
import bon.jo.test.HtmlRep._
import org.scalajs.dom.{console, raw}
import org.scalajs.dom.experimental.URLSearchParams
import org.scalajs.dom.html.{Div, Element, Input, Select, Span, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLUListElement}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}
import scala.xml.Elem






class ViewsImpl(implicit executionContext: ExecutionContext) {
//  implicit val idMemo: Id[Entities.Memo] = m => "m" + m.id.getOrElse(m.title)
//  implicit val idK: Id[KeyWord] = m => "k" + m.id.getOrElse(m.value)
//  implicit val idMemoKw: Id[Entities.MemoKeywords] = m => "mk" + m.memo.id.getOrElse(m.memo.title)

  val viewsDef: ViewsDef = ViewsDef()

  import viewsDef.memoXml
  import viewsDef.keyWord

  object mCtx extends MemoCtxView


  val keywWordI: Input = i

  object keyWordView extends SimpleView[Entities.KeyWord,Nothing](() => $va div ($va span($t("titre :")
    , keywWordI))) {
    def fillFromService: Future[Iterable[KeyWord]] = Daos.keyWordDao.readAll()
      .map(m => {
        m.foreach(+=)
        m
      })
  }

  def addKw: Unit =
    keyWordView.btnInput.html.onclick = _ => {
      val m = Entities.KeyWord(None, keywWordI.value)
      val req: Future[Unit] = Daos.keyWordDao.create(m).map(o => o.foreach(keyWordView.+=))
      req.onComplete {
        case Failure(exception) => throw (exception)
        case Success(_) =>
      }

    }


}


