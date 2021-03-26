package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords}
import bon.jo.test.SimpleView.{dci, i}
import org.scalajs.dom.html.{Div, Input}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.xml.Elem
import bon.jo.test.XmlRep.IdXmlRep
class ViewsImpl(implicit executionContext: ExecutionContext) {
  implicit val idMemo: Id[Entities.Memo] = m => "m" + m.id.getOrElse(m.title)
  implicit val idK: Id[KeyWord] = m => "k" + m.id.getOrElse(m.value)
  implicit val idMemoKw: Id[Entities.MemoKeywords] = m => "mk" + m.memo.id.getOrElse(m.memo.title)

  val viewsDef:ViewsDef = ViewsDef.apply

  import viewsDef._

  class MemoCtxView {
    val tInput: dci = i
    val contentInput: dci = i

    def newMemo = new Entities.Memo(tInput.html.value, contentInput.html.value)
  }

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


  object memoKeywWordtx extends MemoCtxView

  object listView extends DomCpnt[Div] {
    override def xml: Elem = <div id={id}></div>


  }

  def memoKeywWord(proposep: Propose[KeyWord, Input]) = new SimpleView[Entities.MemoKeywords](() =>
    <div>titre :
      {memoKeywWordtx.tInput.xml}<div>Content :</div>
      <div>
        {memoKeywWordtx.contentInput.xml}
      </div>{listView.xml}<div>KeyWord :</div>{proposep.xml}
    </div>) {
    def fillFromService: Future[Unit] = Daos.memoKeyWord.readAll()
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

  def addMKw(memoKeywWord: SimpleView[MemoKeywords],iterable: Iterable[KeyWord]) =
    memoKeywWord.btnInput.html.onclick = _ => {

      val m = Entities.MemoKeywords(memoKeywWordtx.newMemo, iterable.toSet)
      val req: Future[Unit] = Daos.memoKeyWord.create(m).map(o => o.foreach(memoKeywWord.+=))
      req.onComplete {
        case Failure(exception) => throw (exception)
        case Success(value) =>
      }


    }

}
