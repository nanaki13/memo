package bon.jo.memo.ui

import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords}
import HTMLDef.{$t, $va, HtmlOps}
import HtmlRep.HtmlRepParam
import org.scalajs.dom.experimental.URLSearchParams
import org.scalajs.dom.html.{Div, Input}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class MemoKeyWordViewListCreate(val propose: Propose[KeyWord, Input], listView: Div,
                                val memoKeywWordtx: MemoCtxView,addMemo : (Entities.MemoKeywords)=>Unit)
                               (implicit executionContext: ExecutionContext)
  extends SimpleView[Entities.MemoKeywords](() =>
    ($va div(
      $t span "titre",
      memoKeywWordtx.tInput,
      $t span "type :",
      memoKeywWordtx.memoType,
      $t div "content",
      $va div(memoKeywWordtx.contentInput, memoKeywWordtx.memoList.html),
      listView,
      $t div "KeyWord : ",
      propose.html
    )) := ( _._class="col" ) ,addMemo) {




  def addEventNewMemoKeyWord(iterable: Iterable[KeyWord]): Unit =


    btnInput.$click {
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
