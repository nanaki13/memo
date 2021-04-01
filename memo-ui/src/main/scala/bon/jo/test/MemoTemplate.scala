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


  override def xml: Node = <div id="root">
    {target match {
      case Target.MemoCreation => memoKeywWord.xml
      case Target.KeyWordK => view.keyWordView.xml
      case Target._404 =>
      case Target.ReadMemo(id) => <div>On veut lire
        {id}
      </div>
    }}
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

  override def init(p: HTMLElement): Unit = {

    target match {
      case Target.MemoCreation => memoCreationLoad()
      case Target.KeyWordK =>
        view.keyWordView.fillFromService.foreach(_ => view.addKw)
      case Target.ReadMemo(id) =>
        Daos.memoKeyWord.read(id).foreach {
          case Some(value) =>
            import view.viewsDef.memoKeyWordXml
            var memoListIpnutR : Option[MemoListIpnutR] = None
            val html = me.addChild[Div](value.xml[MemoListIpnutR](memoListIpnutR= _))
            lazy val ctx = new MemoCtxView
            memoListIpnutR.foreach(_.addEvent())
            html.getElementsByClassName("btn-save").map(_.asInstanceOf[Button]).foreach(b => {
              b.e.onclick { _ =>
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
              b.e.onclick { _ =>
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
                    a.parentElement.addChild(ctx.memoList.xml)
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
          case None =>
        }
      case Target._404 => _404Load()
    }

    def _404Load() = {
      p.clear()
      p.addChild[raw.HTMLHeadingElement](<h1>page non trouvé</h1>)
    }


    def memoCreationLoad(): Unit = {
      memoKeywWord.memoKeywWordtx.memoList.addEvent()
      memoKeywWord.memoKeywWordtx.memoType.selectFirst
      memoKeywWord.memoKeywWordtx.makeSwitchView()
      memoKeywWord.fillFromService.onComplete {
        case Failure(exception) => throw exception
        case Success(_) =>
      }
      Daos.keyWordDao.readAll().map(kws => {
        memoKeywWord.propose.addAll(kws)
        memoKeywWord.propose.createEvent()
        val htmlI = memoKeywWord.propose.ioHtml.html
        val eventAdd = htmlI.e
        eventAdd.onkeyup {
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

object Paths {

  import Routing._

  val pCreationMemo: Path = "app" / "memo"
  val pCreationKW: Path = "app" / "keyword"
}


object Routing {

  type PF[A] = PartialFunction[Routing.Path, A]


  def create[A](partialFunction: PartialFunction[Routing.Path, A])(implicit _404: A): Routing.Path => A = {
    partialFunction orElse { case _ => _404 }
  }

  case class Path(str: List[String]) {
    def matches[R](list: PathMatchList[R]): Option[R] = {
      if (str.size != list.str.size) {
        None
      } else {
        list.tr(str.zip(list.str).filter(e => e._2.variable && e._2.matches(e._1)).map(e => e._2.extract(e._1)))
      }
    }

    def /(s: String): Path = copy(str = str :+ s)

    def /[A](s: PathMatcher[_])(implicit tp: List[Any] => Option[A]): PathMatchList[A] = PathMatchList(str.map(StrictMatch.apply) :+ s)(tp)
  }

  case class PathMatchList[R](str: List[PathMatcher[_]])(tp: List[Any] => Option[R]) {
    def tr(value: List[Any]): Option[R] = tp(value)

  }

  trait PathMatcher[A] {
    def matches(str: String): Boolean

    def extract(str: String): A

    def variable: Boolean
  }

  trait PathMatcherString extends PathMatcher[String] {
    override def extract(str: String): String = str
  }

  case class StrictMatch(strRef: String) extends PathMatcherString {
    override def matches(str: String): Boolean = strRef == str

    override def variable: Boolean = false
  }

  object IntPath extends PathMatcher[Int] {
    override def matches(str: String): Boolean = {
      Try(str.toInt) match {
        case Failure(_) => false
        case Success(_) => true
      }
    }


    override def variable: Boolean = true

    override def extract(str: String): Int = str.toInt
  }

  implicit class PFromStr(str: String) {
    def /(o: String): Path = Path(List(str, o))
  }

  object Path {
    def apply(str: String): Path = Path(str.split("/").toList.filter(_.nonEmpty))

  }

  def urlPath: Routing.Path = Path(scalajs.js.URIUtils.decodeURI(window.location.pathname))
}
