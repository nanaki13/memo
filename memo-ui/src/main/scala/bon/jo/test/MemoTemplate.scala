package bon.jo.test

import bon.jo.app.User
import bon.jo.game.html.Template
import bon.jo.html.DomShell._
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Entities.{KeyWord, MemoKeywords}
import bon.jo.test.XmlRep.{IdXmlRep, PrXmlId}
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{URL, console, raw, window}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scala.xml.Node


import scalajs.js

sealed trait Target

case object MemoCreation extends Target

case object _404 extends Target

case class MemoTemplate(user: User)(implicit ec: ExecutionContext) extends Template with XmlTemplate {


  console.log(window.location.pathname)


  case class P(str: List[String]) {
    def /(s: String) = copy(str = str :+ s)
  }

  object P {
    def apply(str: String): P = new P(str.split("/").toList.filter(_.nonEmpty))
    implicit class PFromStr(str: String) {
      def /(o: String): P = P(List(str, o))
    }
  }

  import P._

  val view = new ViewsImpl()
  val urlPath = P(scalajs.js.URIUtils.decodeURI(window.location.pathname))

  console.log(urlPath.toString)

  object Paths {
    val pCreation: P = "app" / "memo"
  }


  val target: Target = urlPath match {
    case Paths.pCreation => MemoCreation
    case _ => _404
  }


  val currentKeyWord = scala.collection.mutable.ListBuffer[KeyWord]()

  def addToCurrentKW(keyWord: KeyWord, listView: DomCpnt[Div])(implicit v: IdXmlRep[KeyWord]): raw.Node = {
    currentKeyWord.addOne(keyWord)
    listView.html.appendChild(keyWord.newHtml)
  }

  type ProposeView = (Propose[KeyWord, Input], SimpleView[MemoKeywords])
  private val (propose, memoKeywWord): ProposeView = view.viewsDef.keyWord.other("pr") {
    implicit other =>
      val propose = new Propose[KeyWord, Input](ListBuffer(),
        new IOHtml[Input, KeyWord](id => {
          <input id={id}></input>
        }, input => KeyWord(None, input.value)), Daos.keyWordDao.create, addToCurrentKW(_, view.listView))

      (propose, view.memoKeywWord(propose))
  }


  override def xml: Node = <div id="root">
    {memoKeywWord.xml}
  </div>


  override def init(p: HTMLElement): Unit = {
    target match {
      case MemoCreation => memoCreationLoad()
      case _404 => _404Load()
    }

    def _404Load() = {
      p.clear()
      p.addChild[raw.HTMLHeadingElement](<h1>page non trouvé</h1>)
    }

    def memoCreationLoad() = {
      memoKeywWord.fillFromService.onComplete {
        case Failure(exception) => throw (exception)
        case Success(value) =>
      }
      Daos.keyWordDao.readAll().map(kws => {
        propose.addAll(kws)
        propose.createEvent()
        val htmlI = propose.ioHtml.html
        val eventAdd = htmlI.e
        eventAdd.onkeyup {
          _ =>
            console.log(htmlI.value)

            propose.doFilter(!htmlI.value.trim.isEmpty && _.value.contains(htmlI.value))
        }

      }).onComplete {
        case Failure(exception) => throw (exception)
        case Success(value) =>
      }
      view.addMKw(memoKeywWord, currentKeyWord)
    }
  }
}
