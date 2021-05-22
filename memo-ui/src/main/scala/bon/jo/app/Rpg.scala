package bon.jo.app

import bon.jo.app.Export.{PersoJS, WeaponJS}
import bon.jo.dao.LocalJsDao.MappedDao
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $ref, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.PrXmlId
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.dao.{PersoDao, WeaponDao}
import bon.jo.rpg.raw.Action
import bon.jo.rpg.stat.Perso.WithUI
import bon.jo.rpg.stat.raw.{Perso, Weapon}
import bon.jo.util.Ec
import org.scalajs.dom.document
import org.scalajs.dom.html.{Button, Div}

import scala.collection.mutable.ListBuffer










trait Rpg extends Ec with ArmesPage with RpgSimu {
  def createButton(addRandomButton: Button): Unit


  val onChangePage = ListBuffer[()=>Unit]()
  val weaponDao: MappedDao[WeaponJS, Weapon] with WeaponDao
  val persoDao: MappedDao[PersoJS, Perso] with PersoDao

  val deckCreation: Div = {
    println("deckCreation")
    $c.div[Div]
  }


  var cpntMap: Map[Int, (Perso, PerCpnt)] = _

  val timeLine = TimeLineParam(0, 200, 260)

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
    implicit val repPerso: HtmlUi.PersoRep.type = HtmlUi.PersoRep
    implicit val actionPerso: HtmlRep[Action, ImuutableHtmlCpnt] = HtmlUi.acctRep
    val linkedUI = new WithUI()
    val cpnt = timeLine.timedObjs.map(_.value).map(_.asInstanceOf[Perso]).map(e => e -> e.html)
    cpntMap = cpnt.map(e => e._1.id -> e).toMap

    root.style.maxWidth = "80%"
    cpnt.flatMap(_._2.get).foreach(e => root += e)

    ui.choice.clear()
    ui.messageDiv.clear()
    root.appendChild(ui.choice)
    root.appendChild(ui.messageDiv)
    val cpntTimeLine = new TimeLineCpnt(timeLine, linkedUI)
    root.appendChild(cpntTimeLine.tlView)
    cpntTimeLine.tlView.$userCanDrag()
    cpntTimeLine.doEvent()
    onChangePage += (() => timeLine.stop())
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