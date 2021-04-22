package bon.jo.test

import bon.jo.html.HtmlEventDef._
import bon.jo.memo.Entities
import bon.jo.memo.Entities.KeyWord
import bon.jo.test.HTMLDef._
import bon.jo.test.SimpleView.i
import org.scalajs.dom.html.Input

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}






class ViewsImpl(implicit executionContext: ExecutionContext) {
//  implicit val idMemo: Id[Entities.Memo] = m => "m" + m.id.getOrElse(m.title)
//  implicit val idK: Id[KeyWord] = m => "k" + m.id.getOrElse(m.value)
//  implicit val idMemoKw: Id[Entities.MemoKeywords] = m => "mk" + m.memo.id.getOrElse(m.memo.title)

  val viewsDef: ViewsDef = ViewsDef()

  import viewsDef.keyWord

  object mCtx extends MemoCtxView


  val keywWordI: Input = i

  object keyWordView extends SimpleView[Entities.KeyWord,Nothing](() => $va div ($va span($t("titre :")
    , keywWordI))) {
//    def fillFromService: Future[Iterable[KeyWord]] = Daos.keyWordDao.readAll()
//      .map(m => {
//        m.foreach(+=)
//        m
//      })
  }

  def addKwEvent: Unit =
    keyWordView.btnInput.$click{ _=>
      val m = Entities.KeyWord(None, keywWordI.value)
      val req: Future[Unit] = Daos.keyWordDao.create(m).map(o => o.foreach(keyWordView.+=))
      req.onComplete {
        case Failure(exception) => throw (exception)
        case Success(_) =>
      }

    }


}


