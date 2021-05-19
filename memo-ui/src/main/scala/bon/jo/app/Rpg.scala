package bon.jo.app

import bon.jo.app.Export.{PersoJS, WeaponJS}
import bon.jo.dao.LocalJsDao
import bon.jo.dao.LocalJsDao.{MappedDao, MappedDaoImpl}
import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.html.HTMLDef.{$c, $ref, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.PrXmlId
import bon.jo.memo.ui.SimpleView
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.dao.{PersoDao, WeaponDao}
import bon.jo.rpg.raw.Action
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.Perso.WithUI
import bon.jo.rpg.stat.raw.{Actor, Perso, Weapon}
import bon.jo.util.{Ec, Mapper}
import org.scalajs.dom.document
import org.scalajs.dom.html.{Button, Div}

import scala.collection.mutable
import scala.concurrent.ExecutionContext










trait Rpg extends Ec with ArmesPage {
  def createButton(addRandomButton: Button): Unit


  val weaponDao: MappedDao[WeaponJS, Weapon] with WeaponDao
  val persoDao: MappedDao[PersoJS, Perso] with PersoDao

  val deckCreation: Div = {
    println("deckCreation")
    $c.div[Div]
  }


  //  val apps = List("app-test-socket", "app-test")
  //
  //  val conf: Map[String, HtmlAppFactory[_]] = Map(
  //    "app-test-socket" -> new HtmlAppFactory[TestSocketTemplate]((app: Div, template: Template) => new TestSocketAppApp(app, template), _ => new TestSocketTemplate),
  //    "app-test" -> new HtmlAppFactory[MemoTemplate]((app: Div, template: Template) => new MemoApp(app, template), q =>  MemoTemplate(user = q))
  //  )
  //  loads(apps)


  //  trait ActionOps[A] {
  //    def resolveAction[B](a : A, action: Action, B : B):Unit
  //  }



  document.body.style.backgroundColor = "#343a40"


  var cpntMap: Map[Int, (Perso, PerCpnt)] = _

  val yl = TimeLineParam(0, 200, 260)

  val root = $ref div {
    d =>
      d._class = "container-fluid"


  }
  document.getElementsByTagName("app-rpg").foreach { e =>
    e.innerHTML = ""
    e += root
  }

  def startRpg = {
    implicit val ui: HtmlUi = HtmlUi.Value
    implicit val repPerso = HtmlUi.PersoRep
    implicit val actionPerso = HtmlUi.acctRep
    val linkedUI = new WithUI()
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

    root.style.maxWidth = "80%"
    cpnt.flatMap(_._2.get).foreach(e => root += e)


    root.appendChild(ui.choice)
    root.appendChild(ui.messageDiv)
    val cpntTimeLine = new TimeLineCpnt(yl, linkedUI)
    root.appendChild(cpntTimeLine.tlView)
    cpntTimeLine.tlView.$userCanDrag()
    cpntTimeLine.doEvent()


  }





  /*initChoiXperso.$click { _ =>
    persosForGame.map(_.read).foreach(e => {
      e.randomWeapon()
      yl.add(e)
    })
    root.clear()
    startRpg
  }*/

}
