package bon.jo.test

import bon.jo.app.User
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.html.DomShell._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoType}
import bon.jo.test.Routing.IntPath
import bon.jo.test.XmlRep._
import org.scalajs.dom.html.{Button, Div, Input}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{console, raw, window}
import HTMLDef._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}
import scala.xml.Node

sealed trait Target extends Product

object Target {

  case object MemoCreation extends Target

  case class ReadMemo(id: Int) extends Target

  case object KeyWordK extends Target

  case object _404 extends Target

}


case class MemoTemplate(user: User)(implicit ec: ExecutionContext) extends Template with XmlTemplate {


  val listView: DomCpnt[Div] = DomCpnt[Div](<div></div>)
  val view = new ViewsImpl()


  val currentKeyWord: mutable.ListBuffer[KeyWord] = scala.collection.mutable.ListBuffer[KeyWord]()
 //

  def addToCurrentKW(keyWord: KeyWord, listView: DomCpnt[Div])(implicit v: XmlRep[KeyWord]): raw.Node = {
    currentKeyWord += keyWord
    implicit val idForCurrent: Id[KeyWord] = view.idK.prefix("curr")
    listView.html.appendChild(keyWord.newHtml)
  }

  import view.{idK, idMemoKw}

  private val memoKeywWord: MemoKeyWordViewListCreate = {
    import view.viewsDef._
    val lView = DomCpnt[Div](<div></div>)
    val propose = new Propose[KeyWord, Input](ListBuffer(),
      new IOHtml[Input, KeyWord](id => {
        <input id={id}></input>
      }, input => KeyWord(None, input.value)), Daos.keyWordDao.create, addToCurrentKW(_, lView))

    new MemoKeyWordViewListCreate(propose, lView, new MemoCtxView)
  }


  override def xml: Node = <div id="root"  class="container">
    {
//    target match {
//      case Target.MemoCreation => memoKeywWord.xml
//      case Target.KeyWordK => view.keyWordView.xml
//      case Target._404 =>
//      case Target.ReadMemo(id) => <div>On veut lire
//        {id}
//      </div>
//    }
    }
  </div>


  implicit val _404: Target = Target._404
  implicit val tp: List[Any] => Option[Int] = {
    case Nil => None
    case a => Some(a.head.asInstanceOf[Int])
  }
  val pathToTarget: Routing.Path => Target = Routing.create {
    case Paths.pCreationMemo => Target.MemoCreation
    case Paths.pCreationKW => Target.KeyWordK
    case o@_ => o.matches(Paths.pCreationMemo / IntPath) match {
      case Some(int) => Target.ReadMemo(int)
      case _ => Target._404
    }
  }

  def target: Target = pathToTarget(Routing.urlPath)

  def addMemo(value: Entities.MemoKeywords): Unit = {
    import view.viewsDef.memoKeyWordXml

    var memoListIpnutR : Option[MemoListIpnutR] = None
    val html = me :+ value.html
    lazy val ctx = new MemoCtxView

    html.getElementsByClassName("btn-save").map(_.asInstanceOf[Button]).foreach(b => {
      b.$click { _ =>
        val ret = value.memo.memoType match {
          case MemoType.Text => value
          case MemoType.Json => value.copy(memo = value.memo.copy(content = JSON.stringify(memoListIpnutR.get.read().pure())))
        }
        Daos.memoKeyWord.update(ret).onComplete {
          case Failure(exception) => console.log(exception);PopUp("Sauvegarde KO")
          case Success(value) => PopUp("Sauvegarde OK")

        }
      }}
    )

    html.getElementsByClassName("btn-edit").map(_.asInstanceOf[Button]).foreach(b => {
      b.$click { _ =>
        val orgText = b.innerText
        if(b.innerText != "save"){
          b.innerText = "save"

          html.getElementsByClassName("a-title").map(_.asInstanceOf[HTMLElement]).foreach { a =>
            a.parentElement.addChild(ctx.tInput.xml)
            ctx.tInput.html.value = value.memo.title
          }
          html.getElementsByClassName("m-content").map(_.asInstanceOf[HTMLElement]).foreach { a =>
            a.parentElement.addChild(ctx.contentInput.xml)
            ctx.contentInput.html.value = value.memo.content
            a.parentElement :+ (ctx.memoList.html)
            ()
          }
          html.getElementsByClassName("m-type").map(_.asInstanceOf[HTMLElement]).foreach { a =>
            a.addChild(ctx.memoType.xml)
            ctx.memoType.select(value.memo.memoType.toString)
          }
          ctx.makeSwitchView()
          ctx.memoList.addEvent()
        }else{
          val nMoemo = ctx.newMemo.copy(value.memo.id)
          Daos.memoKeyWord.update( value.copy(memo = nMoemo)).onComplete {
            case Failure(exception) => console.log(exception)
            case Success(value) => PopUp("Sauvegarde OK")
              b.innerText = orgText
          }
        }


      }
    })
    ()
  }

  override def init(p: HTMLElement): Unit = {

    target match {
      case Target.MemoCreation => memoCreationLoad()
      case Target.KeyWordK =>
        view.keyWordView.fillFromService.foreach(_ => view.addKw)
      case Target.ReadMemo(id) =>
        Daos.memoKeyWord.read(id).foreach {
          case Some(value) =>
            addMemo(value)
          case None =>
        }
      case Target._404 => _404Load()
    }

    p.appendChild(HTMLDef.htmlTest)
    def _404Load() = {
      p.clear()
      p.addChild[raw.HTMLHeadingElement](<h1>page non trouvé</h1>)
    }


    def memoCreationLoad(): Unit = {
      memoKeywWord.memoKeywWordtx.memoList.addEvent()
      memoKeywWord.memoKeywWordtx.memoType.selectFirst()
      memoKeywWord.memoKeywWordtx.makeSwitchView()
      memoKeywWord.fillFromService.onComplete {
        case Failure(exception) => throw exception
        case Success(d) => d.foreach(addMemo)
      }
      Daos.keyWordDao.readAll().map(kws => {
        memoKeywWord.propose.addAll(kws)
        memoKeywWord.propose.createEvent()
        val htmlI = memoKeywWord.propose.ioHtml.html

        htmlI.$keyup {
          _ =>
            memoKeywWord.propose.doFilter(htmlI.value.trim.nonEmpty && _.value.contains(htmlI.value))
        }

      }).onComplete {
        case Failure(exception) => throw exception
        case Success(_) =>
      }
      memoKeywWord.addMKw(currentKeyWord)
    }
  }
}





