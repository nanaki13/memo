package bon.jo.test

import java.util.UUID

import bon.jo.app.User
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.memo.Entities
import bon.jo.memo.Entities.KeyWord
import bon.jo.test.SimpleView.{dci, i}
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.{Element, HTMLElement}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.xml.Node
import XmlRep._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import org.scalajs.dom.console

case class MemoTemplate(user: User)(implicit ec: ExecutionContext) extends Template with XmlTemplate {


  val tInput: dci = i
  val contentInput: dci = i
  implicit val idMemo: Id[Entities.Memo] =m => "m" + m.id.getOrElse(m.title)
  implicit val idK: Id[KeyWord] = m => "k" + m.id.getOrElse(m.value)
  implicit val idMemoKw: Id[Entities.MemoKeywords] =m => "mk" + m.memo.id.getOrElse(m.memo.title)
  console.log(idMemo,idK,idMemoKw)
  val viewsDef: ViewsDef =  ViewsDef.apply

  import viewsDef._

  object memoView extends SimpleView[Entities.Memo](() => <div>titre :
    {tInput.xml}
    contenu :
    {contentInput.xml}
  </div>) {
    def fillFromService = Daos.memoDao.readAll()
      .map(m => {
        m.foreach(+=)
      })
  }

  val keywWordI: dci = i

  object keyWordView extends SimpleView[Entities.KeyWord](() => <div>keyWord :
    {keywWordI.xml}
  </div>) {
    def fillFromService = Daos.keyWordDao.readAll()
      .map(m => {
        m.foreach(+=)
      })
  }


  val memoObj: dci = i

  object memoKeywWord extends SimpleView[Entities.MemoKeywords](() => <div>content :
    {memoObj.xml}
  </div>) {
    def fillFromService = Daos.memoKeyWord.readAll()
      .map(m => {
        m.foreach(+=)
      })
  }


  override def xml: Node = <div id="root">
    {memoKeywWord.xml}{propose.xml}
  </div>

  def addMemoLisner =
    memoView.btnInput.html.onclick = _ => {
      val m = new Entities.Memo(tInput.html.value, contentInput.html.value)
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


  val propose = new Propose[KeyWord, Input](ListBuffer(),
    new IOHtml[Input, KeyWord](id => {
      <input id={id}></input>
    }, input => KeyWord(None, input.value)))

  override def init(p: HTMLElement): Unit = {


    //    memoKeywWord.fillFromService.onComplete {
    //      case Failure(exception) => throw(exception)
    //      case Success(value) =>
    //    }
    Daos.keyWordDao.readAll().map(kws => {
      propose.addAll(kws)
      propose.createEvent()
      val htmlI = propose.ioHtml.html
      val eventAdd = htmlI.e
      eventAdd.onkeyup{
        _ =>
          console.log(htmlI.value)

          propose.doFilter(!htmlI.value.trim.isEmpty && _.value.contains(htmlI.value))
      }

    }).onComplete {
      case Failure(exception) => throw (exception)
      case Success(value) =>
    }
    //   keyWordView.fillFromService

  }
}
