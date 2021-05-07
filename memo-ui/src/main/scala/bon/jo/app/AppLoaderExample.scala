package bon.jo.app


import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.html.HTMLDef.{$c, $ref, $t, $va, HtmlOps}
import bon.jo.memo.ui.HtmlRep.PrXmlId
import bon.jo.rpg.raw.BattleTimeLine.TimeLineParam
import bon.jo.rpg.raw.DoActionTrait.WithAction
import bon.jo.rpg.raw._
import bon.jo.rpg.stat.raw.Perso.WithUI
import bon.jo.rpg.stat.raw._
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{console, document, window}

import scala.concurrent.ExecutionContext.Implicits._

object AppLoaderExample extends App {

  //  val apps = List("app-test-socket", "app-test")
  //
  //  val conf: Map[String, HtmlAppFactory[_]] = Map(
  //    "app-test-socket" -> new HtmlAppFactory[TestSocketTemplate]((app: Div, template: Template) => new TestSocketAppApp(app, template), _ => new TestSocketTemplate),
  //    "app-test" -> new HtmlAppFactory[MemoTemplate]((app: Div, template: Template) => new MemoApp(app, template), q =>  MemoTemplate(user = q))
  //  )
  //  loads(apps)

  var id = 0

  def getid() = {
    id += 1
    id
  }






  //  trait ActionOps[A] {
  //    def resolveAction[B](a : A, action: Action, B : B):Unit
  //  }

  println("Avant P1 !")





  val p1 = Actor.randomActor(Perso("Bob",_))
  println("Avant P1 !")
  p1.leftHand = Some(Actor.randomWeapon())
  p1.leftHand.foreach(e => e.action = e.action :+ Action.Soin)
  val p2 = Perso("Bill",AnyRefBaseStat.randomInt(50,25))
  val e1 = Perso("Mechant 1",AnyRefBaseStat.randomInt(50,25))
  val e2 = Perso("Mechant 2",AnyRefBaseStat.randomInt(50,25))

  val l = List(p1,p2,e1,e2)
  l.foreach(_.randomWeapon())
  val yl = TimeLineParam(0, 200, 260)
  yl.add(p1)
  yl.add(p2)
  yl.add(e1)
  yl.add(e2)


  implicit val ui: HtmlUi.Value.type = HtmlUi.Value
  val linkedUI = new WithUI()











  import HtmlUi._
  val cpnt = yl.timedObjs.map(_.value).map(_.asInstanceOf[Perso]).map(e => e -> e.html)
  val cpntMap = cpnt.map(e => e._1.id -> e).toMap
  val actionChoice: Seq[(Action, ImuutableHtmlCpnt)] = Action.values.map(e => (e, e.html))
  //document.body.clear()
 // document.body.classList.add( " bg-dark")
  val root = $ref div{
    d =>
      d._class = "container-fluid bg-dark"
      d.style.height=s"${window.innerHeight}px"

  }
  document.getElementsByTagName("app-rpg").foreach{e =>
    e.innerHTML=""
    e += root
  }
  val row = $ref div {_._class = "row"}
  def col =$ref div {e =>
    e._class = "col-2"
    row+=e
  }

  root+=row
  cpnt.flatMap(_._2.get).foreach(e =>col += e)


  root.appendChild(ui.choice)
  root.appendChild(ui.messageDiv)
 val cpntTimeLine =  new TimeLineCpnt(yl,linkedUI)
  root.appendChild(cpntTimeLine.tlView)
  cpntTimeLine.doEvent()


}




class TimeLineCpnt(val el : TimeLineParam,val withUI: WithUI){
  import withUI.acImpl
  implicit val ui = withUI.o.ui
  val tlView :Div= $c.div
  tlView.style.position="absolute"
  tlView.style.top = "10px"
  tlView.style.right = s"0"
  val htmlName = el.timedObjs.map(_.simpleName).map(t => $t span t)
  htmlName.map {
    e =>

      e.style.position = "absolute"
      e
  }.foreach(e => tlView.appendChild({
    val in = $va div (e)
    in.style.height = "1em"
    val s1 : Span = $c.span
    s1.style.width=s"${el.chooseAction}px"
    s1.style.backgroundColor = "blue"
    s1.style.height = "1em"
    s1.style.display="inline-block"
    val s2 : Span = $c.span
    s2.style.width=s"${el.action - el.chooseAction}px"
    s2.style.backgroundColor = "red"
    s2.style.height = "1em"
    s2.style.display="inline-block"
    val s3 : Span = $c.span
    s3.style.width=s"12em"
    s3.style.backgroundColor = "green"
    s3.style.opacity = "0"
    s3.style.height = "1em"
    s3.style.display="inline-block"
    in ++= (s1,s2,s3)
    in
  }))

  def update = {
    htmlName zip el.timedObjs foreach{
      case (element, value) =>
        element.style.left=value.pos.toString+"px"
    }
  }
  def doEvent() = {
    lazy val int : Int = window.setInterval(() => {
      console.log("loop")
      if(el.pause == 0){
        el.nextState
        update
      }else{
        window.clearInterval(int)
      }

      //    cpnt.foreach {
      //      case (perso, cpnt) => cpnt.update(Some(perso))
      //    }
    }, 25)
    int

  }
  el.resume =  doEvent _

}



