package bon.jo.test

import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords, MemoType}
import bon.jo.test.SimpleView.{dci, dcselect, dcta, i, s, ta}
import org.scalajs.dom.html.{Div, Input}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node}
import bon.jo.test.XmlRep._
import org.scalajs.dom.experimental.URLSearchParams

class MemoCtxView {

  implicit val idXmlRep: IdXmlRep[MemoList] = XmlRepImpl[MemoList](m => <div>
  </div>, m => "l")


  object memoListIpnutR{def apply() = new memoListIpnutR}
  class memoListIpnutR extends DomCpnt[Div] {
    val tInput: dci = i
    val data = new MemoList(Nil)
    def xml = <div id={id}>
      {new MemoList(Nil).xml}
      tInput
    </div>

  }

  val memoList: DomCpnt[Div] = memoListIpnutR()

  val tInput: dci = i
  val contentInput: dcta = ta
  val memoType: dcselect = s[MemoType]




  def newMemo = new Entities.Memo(tInput.html.value, contentInput.html.value, MemoType(memoType.html.value))
}

class ViewsImpl(implicit executionContext: ExecutionContext) {
  implicit val idMemo: Id[Entities.Memo] = m => "m" + m.id.getOrElse(m.title)
  implicit val idK: Id[KeyWord] = m => "k" + m.id.getOrElse(m.value)
  implicit val idMemoKw: Id[Entities.MemoKeywords] = m => "mk" + m.memo.id.getOrElse(m.memo.title)

  val viewsDef: ViewsDef = ViewsDef.apply

  import viewsDef._


  object mCtx extends MemoCtxView

  object memoView extends SimpleView[Entities.Memo](() => <div>titre :
    {mCtx.tInput.xml}
    contenu :
    {mCtx.contentInput.xml}
  </div>) {
    def fillFromService: Future[Unit] = Daos.memoDao.readAll()
      .map(m => {
        m.foreach(+=)
      })
  }

  val keywWordI: dci = i

  object keyWordView extends SimpleView[Entities.KeyWord](() => <div>keyWord :
    {keywWordI.xml}
  </div>) {
    def fillFromService: Future[Unit] = Daos.keyWordDao.readAll()
      .map(m => {
        m.foreach(+=)
      })
  }

  def addMemoLisner =
    memoView.btnInput.html.onclick = _ => {
      val m = mCtx.newMemo
      val req: Future[Unit] = Daos.memoDao.create(m).map(o => o.foreach(memoView.+=))
      req.onComplete {
        case Failure(exception) => throw (exception)
        case Success(value) =>
      }


    }

  def addKw =
    keyWordView.btnInput.html.onclick = _ => {
      val m = Entities.KeyWord(None, keywWordI.html.value)
      val req: Future[Unit] = Daos.keyWordDao.create(m).map(o => o.foreach(keyWordView.+=))
      req.onComplete {
        case Failure(exception) => throw (exception)
        case Success(value) =>
      }

    }
}


class MemoKeyWordViewListCreate(val propose: Propose[KeyWord, Input], listView: DomCpnt[Div], val memoKeywWordtx: MemoCtxView)
                               (implicit idXmlRep: IdXmlRep[MemoKeywords], executionContext: ExecutionContext)
  extends SimpleView[Entities.MemoKeywords](() =>
    <div>titre
      {memoKeywWordtx.tInput.xml}<div>type :
      {memoKeywWordtx.memoType.xml}
    </div>
      <div>Content :</div>
      <div>
        {memoKeywWordtx.contentInput.xml}{memoKeywWordtx.memoList.xml}
      </div>{listView.xml}<div>KeyWord :</div>{propose.xml}
    </div>) {


  val searchParams = new URLSearchParams(org.scalajs.dom.window.location.search)
  val (limit, offset) = Try {
    (Option(searchParams.get("limit")).map(_.toInt).getOrElse(-1)
      , Option(searchParams.get("from")).map(_.toInt).getOrElse(-1))
  } match {
    case Failure(exception) => (-1, -1)
    case Success(value) => value
  }

  def fillFromService: Future[Unit] = Daos.memoKeyWord.readAll(limit = limit, offset = offset)
    .map(m => {
      m.foreach(+=)
    })

  def addMKw(iterable: Iterable[KeyWord]) =


    btnInput.html.e.onclick {
      _ => {
        val m = Entities.MemoKeywords(memoKeywWordtx.newMemo, iterable.toSet)
        val req: Future[Unit] = Daos.memoKeyWord.create(m).map(o => o.foreach(+=))
        req.onComplete {
          case Failure(exception) => throw (exception)
          case Success(value) =>
        }
      }
    }
}
