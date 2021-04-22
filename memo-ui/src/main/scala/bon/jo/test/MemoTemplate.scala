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
import bon.jo.test.HtmlRep._
import org.scalajs.dom.html.{Button, Div, Input}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{console, raw, window}
import HTMLDef.{$c, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}
import scala.xml.Node
import SimpleView.DSelect
import bon.jo.test.ViewsDef.ProposeInput


case class MemoTemplate(user: User)(implicit ec: ExecutionContext) extends Template with XmlTemplate {


  val view = new ViewsImpl()


  val currentKeyWord: mutable.ListBuffer[KeyWord] = scala.collection.mutable.ListBuffer[KeyWord]()
  //

  def addToCurrentKW(keyWord: KeyWord, listView: Div)(implicit v: HtmlRep[KeyWord]): raw.Node = {
    currentKeyWord += keyWord

    listView.html.appendChild(keyWord.html)
  }


  override def xml: Node = <div id="root" class="container">

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
  def deleteEvent(kw : KeyWord,htmlKw : HTMLElement)(implicit lubber: ListBuffer[KeyWord]): Unit ={
    htmlKw.$classSelect(ViewsDef.closeClass).foreach { btnClose =>
      btnClose.asInstanceOf[HTMLElement].$click{
        _ =>
          lubber -= kw
          htmlKw.removeFromDom()
      }
    }
  }
  def addMemo(value: Entities.MemoKeywords, allKeyWord: Iterable[KeyWord]): Unit = {
    import view.viewsDef.keyWord
    val ctx = new MemoCtxView
    val cpnt = new view.viewsDef.MKCpnt(value, ctx.memoList)
    implicit val keyWordsBuffer: ListBuffer[KeyWord] = ListBuffer.from(value.keyWords.toList)


    val propose = new ProposeInput[KeyWord](_.value,"Creer/Chercher tags")(ListBuffer(),
      ViewsDef.kwIO(), Daos.keyWordDao.create, selected => {
        keyWordsBuffer += selected
        val htmlKW  =selected.html
        cpnt.kwDiv += htmlKW
        deleteEvent(selected, htmlKW)

      })
    propose.addAll(allKeyWord)
    val html = cpnt.html
    keyWordsBuffer zip cpnt.kwDiv.$classSelect(ViewsDef.kwClass).map(_.asInstanceOf[HTMLElement]) foreach {
         deleteEvent _ tupled _
    }


    me += html
    me += propose.html

    html.$classSelect.`btn-save`.map(_.asInstanceOf[Button]).foreach(b => {
      b.$click { _ =>
        val ret = value.memo.memoType match {
          case MemoType.Text => value
          case MemoType.Json => value.copy(memo = value.memo.copy(
            content = JSON.stringify(ctx.memoList.read().pure()))
            , keyWords = keyWordsBuffer.toSet
          )
        }
        Daos.memoKeyWord.update(ret).onComplete {
          case Failure(exception) => console.log(exception); PopUp("Sauvegarde KO")
          case Success(value) => PopUp("Sauvegarde OK")

        }
      }
    }
    )

    html.$classSelect.`btn-edit`.map(_.asInstanceOf[Button]).foreach(b => {
      val orgText = b.innerText
      b.$click { _ =>

        if (b.innerText != "save") {
          b.innerText = "save"

          html.$classSelect.`a-title`.map(_.asInstanceOf[HTMLElement]).foreach { a =>
            a.parentElement += ctx.tInput
            ctx.tInput.value = value.memo.title
          }
          html.$classSelect.`m-content`.map(_.asInstanceOf[HTMLElement]).foreach { a =>
            a.parentElement += ctx.contentInput
            ctx.contentInput.value = value.memo.content
            a.parentElement += (ctx.memoList.html)
            ()
          }
          html.$classSelect.`m-type`.map(_.asInstanceOf[HTMLElement]).foreach { a =>
            a += (ctx.memoType)
            ctx.memoType.select(value.memo.memoType.toString)
          }
          ctx.makeSwitchView()
          ctx.memoList.addEvent()
        } else {
          val nMoemo = ctx.newMemo.copy(value.memo.id)
          Daos.memoKeyWord.update(value.copy(memo = nMoemo)).onComplete {
            case Failure(exception) => console.log(exception)
            case Success(value) =>
              PopUp("Sauvegarde OK")
              b.innerText = orgText
              ctx.memoList.html.removeFromDom()
              ctx.memoType.removeFromDom()
              ctx.tInput.removeFromDom()
              ctx.contentInput.removeFromDom()
          }

        }


      }
    })
    ()
  }


  override def init(p: HTMLElement): Unit = {
    implicit val pa: HTMLElement = p
    Daos.keyWordDao.readAll().map(implicit allKeyWord => {
      target match {
        case Target.MemoCreation => memoCreationLoad()
        case Target.KeyWordK =>
          p.appendChild(view.keyWordView.cpnt)
          allKeyWord.foreach { kw =>
            view.keyWordView.+=(kw)
            view.addKwEvent
          }
        case Target.ReadMemo(id) =>
          Daos.memoKeyWord.read(id).foreach {
            case Some(value) =>
              addMemo(value, allKeyWord)
            case None =>
          }
        case Target._404 => _404Load()
      }

    })


  }


  def _404Load()(implicit p: HTMLElement) = {
    p.clear()
    p.addChild[raw.HTMLHeadingElement](<h1>page non trouvé</h1>)
  }


  def memoCreationLoad()(implicit p: HTMLElement, kws: Iterable[KeyWord]): Unit = {
    val memoKeywWord: MemoKeyWordViewListCreate = {
      import view.viewsDef._
      val lView: Div = $c.div

      val propose = new ProposeInput[KeyWord](_.value,"Creer/Chercher tags")(ListBuffer(),
        ViewsDef.kwIO(), Daos.keyWordDao.create, addToCurrentKW(_, lView))

      new MemoKeyWordViewListCreate(propose, lView, new MemoCtxView)
    }
    p.appendChild(memoKeywWord.cpnt);
    memoKeywWord.memoKeywWordtx.memoType.selectFirst()

    memoKeywWord.memoKeywWordtx.makeSwitchView()
    memoKeywWord.memoKeywWordtx.memoList.addEvent()
    memoKeywWord.callService.onComplete {
      case Failure(exception) => throw exception
      case Success(d) => d.foreach(addMemo(_, kws))
    }
    memoKeywWord.propose.addAll(kws)
    memoKeywWord.addEventNewMemoKeyWord(currentKeyWord)
  }
}





