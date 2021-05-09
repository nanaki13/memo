package bon.jo.app


import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $ref, $t, $va, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.PrXmlId
import bon.jo.memo.ui.SimpleView
import bon.jo.rpg.raw.BattleTimeLine.TimeLineParam
import bon.jo.rpg.raw.DoActionTrait.WithAction
import bon.jo.rpg.raw._
import bon.jo.rpg.stat.raw.Perso.WithUI
import bon.jo.rpg.stat.raw._
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{console, document, window}

import scala.collection.mutable
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


  document.body.style.backgroundColor = "#343a40"

  //  val p1 = Actor.randomActor(Perso(RandomName(),_))
  println("Avant P1 !")
  //  p1.leftHand = Some(Actor.randomWeapon())
  //  p1.leftHand.foreach(e => e.action = e.action :+ Action.Soin)
  //  val p2 = Perso("Bill",AnyRefBaseStat.randomInt(50,25))
  //  val e1 = Perso("Mechant 1",AnyRefBaseStat.randomInt(50,25))
  //  val e2 = Perso("Mechant 2",AnyRefBaseStat.randomInt(50,25))
  var cpntMap: Map[Int, (Perso, PerCpnt)] = _
  //  val l = List(p1,p2,e1,e2)
  //  l.foreach(_.randomWeapon())
  val yl = TimeLineParam(0, 200, 260)
  //  yl.add(p1)
  //  yl.add(p2)
  //  yl.add(e1)
  //  yl.add(e2)
  val root = $ref div {
    d =>
      d._class = "container-fluid"


  }
  document.getElementsByTagName("app-rpg").foreach { e =>
    e.innerHTML = ""
    e += root
  }

  def startRpg = {
    implicit val ui: HtmlUi.Value.type = HtmlUi.Value
    val linkedUI = new WithUI()


    import HtmlUi._
    val cpnt = yl.timedObjs.map(_.value).map(_.asInstanceOf[Perso]).map(e => e -> e.html)
    cpntMap = cpnt.map(e => e._1.id -> e).toMap
    val actionChoice: Seq[(Action, ImuutableHtmlCpnt)] = Action.commonValues.map(e => (e, e.html))
    //document.body.clear()
    // document.body.classList.add( " bg-dark")


//    val row = $ref div {
//      _._class = "row"
//    }
//
//    def col = $ref div { e =>
//      e._class = "col"
//      row += e
//    }

  //  root += row

    root.style.maxWidth="80%"
    cpnt.flatMap(_._2.get).foreach(e => root += e)


    root.appendChild(ui.choice)
    root.appendChild(ui.messageDiv)
    val cpntTimeLine = new TimeLineCpnt(yl, linkedUI)
    root.appendChild(cpntTimeLine.tlView)
    cpntTimeLine.tlView.$userCanDrag()
    cpntTimeLine.doEvent()


  }

  val persosForGame = mutable.ListBuffer.empty[EditPersoPerso]

  def initChoiXperso = {
    import EditPersoPerso._

    val p = Actor.randomActor(Perso(RandomName(), _))

    val persoCpnt = p.htmlp(persosForGame)
    persosForGame += persoCpnt
    val bnt = SimpleView.bsButton("start")
    val deckCreation = $ref div {
      r =>
        r ++= persoCpnt.list
    }
    root ++= List(deckCreation, bnt)
    bnt
  }


  initChoiXperso.$click { _ =>
    persosForGame.map(_.read).foreach(e => {
      e.randomWeapon()
      yl.add(e)
    })
    root.clear()
    startRpg
  }

}








