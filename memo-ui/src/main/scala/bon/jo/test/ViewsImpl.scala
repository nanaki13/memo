package bon.jo.test

import bon.jo.html.DomShell.{$,ExtendedElement, ExtendedHTMLCollection, ExtendedNode}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords, MemoType}
import bon.jo.test.SimpleView.{dci, dcselect, dcta, i, s, ta}
import bon.jo.test.XmlRep._
import org.scalajs.dom.{console, raw}
import org.scalajs.dom.experimental.URLSearchParams
import org.scalajs.dom.html.{Div, Element, Input, Span}
import org.scalajs.dom.raw.{HTMLElement, HTMLUListElement}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}
object MemoListIpnutR{


  def apply() = new MemoListIpnutR}
class MemoListIpnutR( val data :MemoListJS = new MemoList(Nil.toJSArray  )) extends DomCpnt[Div] {
  val tInput: dci = i

  implicit val listElementidXmlRep: XmlRep[ListElementJS] = li => <li>
    {
    if(li.checked){
      <input  id={li.content+"i"} type="checkbox" checked="1"></input>
    }else{
      <input  id={li.content+"i"} type="checkbox" ></input>
    }
    }
    <span id={li.content+"c"}>{li.content}</span><button id={li.content+"d"}>x</button></li>

  implicit val idEl: Id[ListElementJS] = me => s"$id-${me.content}"
  implicit val idXmlRep: XmlRep[MemoListJS] = XmlRepImpl[MemoListJS](m => <ul id={id+"l"}>{m.elements.toList.xml}</ul>)
  implicit val idListElementJS: Id[MemoListJS] = me => s"$id-l"
  lazy val list = $[HTMLUListElement](id+"l")
  def xml = <div id={id}>
    {tInput.xml}
    {data.xml}
  </div>

  def addEvent() = {
    val ev = tInput.html.e
    ev.onAction{
      val el :ListElementJS = new ListElement(tInput.html.value,true)
      list.appendChild(el.newHtml)
      $[HTMLElement](el.content+"d").e.onclick{ _ => el.html.removeFromDom()}
    }
    ev.onkeyup{
      _ =>
        list.children.foreach { e =>
          val lElemntText = readInput(e)
          val show = lElemntText.toLowerCase.contains(tInput.html.value.toLowerCase)
          e.asInstanceOf[HTMLElement].show(show)
        }
    }

  }
  private def readInput(l : raw.Element)= $[Span](l.getAttribute("id")+"c").innerText
  def read():MemoList = {
    new MemoList(list.children.map(l =>  new ListElement ($[Span](l.getAttribute("id")+"c").innerText,$[Input](l.getAttribute("id")+"i").checked) ).map(_.asInstanceOf[ListElementJS]).toJSArray)
  }
}
class MemoCtxView {






  val memoList: MemoListIpnutR = MemoListIpnutR()

  val tInput: dci = i
  val contentInput: dcta = ta
  val memoType: dcselect = s[MemoType]




  def newMemo = {
    val mt = MemoType(memoType.html.value)
    val ret =  mt match {
      case MemoType.Text =>  new Entities.Memo(tInput.html.value, contentInput.html.value, MemoType(memoType.html.value))
      case MemoType.Json =>  new Entities.Memo(tInput.html.value, JSON.stringify(memoList.read().pure()) , MemoType(memoType.html.value))
    }

    ret

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
                               (implicit idXmlRep: XmlRep[MemoKeywords],idM: Id[MemoKeywords], executionContext: ExecutionContext)
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
