package bon.jo.test

import bon.jo.app.RequestHttp.{GET, POST}
import bon.jo.app.User.Visitor
import bon.jo.app.{HtmlApp, User}
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.test.TestView._
import bon.jo.test.XmlRep._
import org.scalajs.dom.html.{Button, Div, Input}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.util.{Failure, Success}
import scala.xml.Node
class Test(template: Template) extends HtmlApp[TestTemplate](template: Template) {




  override def asynStartup(): Future[Unit] = {
    GET.get("http://localhost:8080/memo")
      .map(resp => resp.body[js.Array[js.Dynamic]].map(e => {
        e.map(an => Memo(an.id.asInstanceOf[Int],
          an.title.asInstanceOf[String],an.content.asInstanceOf[String],Visitor))
      })).map( m => m.foreach(z => z.foreach(typedTemplate.+=)))
  }

  override def haveAsynStartup: Boolean = true
}


case class TestTemplate(user: User) extends Template with XmlTemplate{

  val memo : ListBuffer[Memo] = ListBuffer()

  def +=(p: Memo): memo.type = {
    memos.html.appendChild(p.html)
    memo += p
  }

  type dc = DomCpnt[_ <: HTMLElement]
  type dci = DomCpnt[Input]
  def i : dci = DomCpnt[Input](<input></input>)
  def b(title : String):dc = DomCpnt[Button](<button>{title}</button>)
  val tInput : dci = i
  val contentInput  : dci = i
  val btnInput  : dc= b("ajouter")
  val memos  : dc = DomCpnt[Div]( <div>{memo.xml}</div>)
  override def xml: Node = <div id="root">
    {memos.xml}
    titre : {tInput.xml}
    contenu : {contentInput.xml}
    {btnInput.xml}
  </div>



  override def init(p: HTMLElement): Unit = {
    println(memo)
    btnInput.html.classList.add("btn")
    btnInput.html.classList.add("btn-primary")
    btnInput.html.onclick = _ => {
      val m = Memo(0,tInput.html.value,contentInput.html.value,Visitor)

      val req: Future[Unit] =  POST.changeStatut(200).send("http://localhost:8080/memo",js.Dynamic.literal(
        title = m.title,
        content = m.content
      ))(e =>JSON.stringify(e))
        .map( e => {
          e.body[js.Dynamic]
            .map(an => +=(Memo(an.id.asInstanceOf[Int],
              an.title.asInstanceOf[String],an.content.asInstanceOf[String],Visitor)))

        })

      req.onComplete {
        case Failure(exception) => println(exception)
        case Success(_) =>
      }

    }


  }
}


