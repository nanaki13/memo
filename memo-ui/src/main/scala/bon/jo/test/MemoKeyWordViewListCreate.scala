package bon.jo.test

import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords}
import bon.jo.test.HTMLDef.{$t, $va}
import bon.jo.test.HtmlRep.XmlRepParam
import org.scalajs.dom.experimental.URLSearchParams
import org.scalajs.dom.html.{Div, Input}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class MemoKeyWordViewListCreate(val propose: Propose[KeyWord, Input], listView: Div, val memoKeywWordtx: MemoCtxView)
                               (implicit idXmlRep: XmlRepParam[MemoKeywords, MemoListView], executionContext: ExecutionContext)
  extends SimpleView[Entities.MemoKeywords, MemoListView](() =>
    $va div(
      $t span "titre",
      memoKeywWordtx.tInput,
      $t span "type :",
      memoKeywWordtx.memoType,
      $t div "content",
      $va div(memoKeywWordtx.contentInput, memoKeywWordtx.memoList.html),
      listView,
      $t div "KeyWord : ",
      propose.html
    )) {


  val searchParams = new URLSearchParams(org.scalajs.dom.window.location.search)
  val (limit, offset) = Try {
    (Option(searchParams.get("limit")).map(_.toInt).getOrElse(-1)
      , Option(searchParams.get("from")).map(_.toInt).getOrElse(-1))
  } match {
    case Failure(_) => (-1, -1)
    case Success(value) => value
  }

  def callService: Future[Iterable[MemoKeywords]] = Daos.memoKeyWord.readAll(limit = limit, offset = offset)

  def addMKw(iterable: Iterable[KeyWord]): Unit =


    btnInput.$click {
      _ => {
        val m = Entities.MemoKeywords(memoKeywWordtx.newMemo, iterable.toSet)
        val req: Future[Unit] = Daos.memoKeyWord.create(m).map(o => o.foreach(+=(_, Some(new MemoListView()))))
        req.onComplete {
          case Failure(exception) => throw (exception)
          case Success(_) =>
        }
      }
    }
}
