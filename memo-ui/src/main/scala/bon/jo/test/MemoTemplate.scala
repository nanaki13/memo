package bon.jo.test

import bon.jo.app.User
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.html.DomShell._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities
import bon.jo.memo.Entities.{KeyWord, MemoKeywords, MemoType}
import bon.jo.test.Routing.IntPath
import bon.jo.test.HtmlRep._
import org.scalajs.dom.html.{Button, Div, Input}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{console, document, raw, window}
import HTMLDef.{$c, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}
import scala.xml.Node
import SimpleView.DSelect
import bon.jo.test.FindViewDef._
import bon.jo.test.ViewsDef.ProposeInput
import org.scalajs.dom.experimental.URLSearchParams


case class MemoTemplate(user: User)(implicit ec: ExecutionContext) extends Template with XmlTemplate {


  val view = new ViewsImpl()


  val currentKeyWord: mutable.ListBuffer[KeyWord] = scala.collection.mutable.ListBuffer[KeyWord]()
  //

  def addToCurrentKW(keyWord: KeyWord, listView: Div)(implicit v: HtmlRep[KeyWord]): raw.Node = {
    currentKeyWord += keyWord

    listView.html.appendChild(keyWord.html.head)
  }


  override def xml: Node = <div id="root" class="container mt-5 p-2">

  </div>


  implicit val _404: Target = Target._404
  implicit val tp: List[Any] => Option[Int] = {
    case Nil => None
    case a => Some(a.head.asInstanceOf[Int])
  }
  val pathToTarget: Routing.Path => Target = Routing.create {
    case Paths.pMemo => Target.MemoCreation
    case Paths.pFind => Target.FindMemo
    case Paths.pCreationKW => Target.KeyWordK
    case o@_ => o.matches(Paths.pMemo / IntPath) match {
      case Some(int) => Target.ReadMemo(int)
      case _ => Target._404
    }
  }

  def target: Target = pathToTarget(Routing.urlPath)



  def addMemo(value: Entities.MemoKeywords): Unit = {
    import view.viewsDef.KewWordHtml.WithClose._

    val cpnt = new view.viewsDef.MKCpnt(value,proposeView)
    implicit val keyWordsBuffer: ListBuffer[KeyWord] = ListBuffer.from(value.keyWords.toList)

//    def addKeyWord(selected: KeyWord): Unit = {
//      keyWordsBuffer += selected
//      val htmlKW = selected.html.list
//      cpnt.kwDiv ++= htmlKW
//      deleteEvent(selected, htmlKW.head)
//
//    }

//    val propose = new ProposeInput[KeyWord](_.value, "Creer/Chercher tags")(
//      ViewsDef.kwIO(), Daos.keyWordDao.create, proposeView, addKeyWord)


//    cpnt.footer ++= propose.html

    val html = cpnt.get


    val div = $l div (html)
    div._class = "card"
    memosCOnr += div


    ()
  }

  object memosCOnr {
    private var cnt = 0
    private var current: HTMLElement = _

    private def div = $ref div { e => e._class = "card-deck pb-1" }

    def clean(): Unit = me.$classSelect("card-deck").foreach(_.removeFromDom())

    def +=(h: HTMLElement): Unit = {
      if (cnt % 3 == 0) {
        current = div
        me += current
      }
      cnt += 1
      current += h
    }
  }


  override def init(p: HTMLElement): Unit = {

    implicit val pa: HTMLElement = p
    Daos.keyWordDao.readAll().map(implicit allKeyWord => {
      proposeView.addAll(allKeyWord)
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
              addMemo(value)

            case None =>
          }
        case Target.FindMemo => findMemo()
        case Target._404 => _404Load()
      }

    })


  }


  def _404Load()(implicit p: HTMLElement) = {
    p.clear()
    p.addChild[raw.HTMLHeadingElement](<h1>page non trouvé</h1>)
  }

  import view.viewsDef._

  private val proposeView = {
    import view.viewsDef.KewWordHtml._
    ProposeView[KeyWord]()
  }
  private val keyWordToHtml = ViewsDef.kwIO()

  def memoCreationLoad()(implicit p: HTMLElement, kws: Iterable[KeyWord]): Unit = {
    val lView: Div = $c.div
    val memoKeywWord: MemoKeyWordViewListCreate = {


      def addToCurrentKW_(keyWord: KeyWord): raw.Node = {
        import view.viewsDef.KewWordHtml.WithClose._
        addToCurrentKW(keyWord, lView)
      }

      val propose = {
        import view.viewsDef.KewWordHtml.WithClose._
        new ProposeInput[KeyWord](_.value, "Creer/Chercher tags")(
          keyWordToHtml, Daos.keyWordDao.create, proposeView,addToCurrentKW_)
      }

      new MemoKeyWordViewListCreate(propose, lView, new MemoCtxView, addMemo)
    }
    p.appendChild(memoKeywWord.cpnt);
    memoKeywWord.memoKeywWordtx.memoType.selectFirst()

    memoKeywWord.memoKeywWordtx.makeSwitchView()
    memoKeywWord.memoKeywWordtx.memoList.addEvent()

    allMemos.onComplete {
      case Failure(exception) => throw exception
      case Success(d) => d.foreach(addMemo)
    }


    memoKeywWord.addEventNewMemoKeyWord(currentKeyWord)
  }


  //implicit val e: FindView.FindViewProvider.type = FindView.FindViewProvider
  def findMemo()(implicit p: HTMLElement, kws: Iterable[KeyWord], ec: ExecutionContext): Unit = {
    p ++= FindParam("Chercher").htmlp(FindViewCtx(this, kws, ec)).list
  }

  val searchParams = new URLSearchParams(org.scalajs.dom.window.location.search)
  val (limit, offset) = Try {
    (Option(searchParams.get("limit")).map(_.toInt).getOrElse(-1)
      , Option(searchParams.get("from")).map(_.toInt).getOrElse(-1))
  } match {
    case Failure(_) => (-1, -1)
    case Success(value) => value
  }

  def allMemos: Future[Iterable[MemoKeywords]] = Daos.memoKeyWord.readAll(limit = limit, offset = offset)
}






