package bon.jo.app

import bon.jo.app.Export.{PersoJS, WeaponJS}
import bon.jo.dao.LocalJsDao.MappedDao
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef.{$c, $ref, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.PrXmlId
import bon.jo.rpg.BattleTimeLine.{TimeLineParam,TimeLineOps}
import bon.jo.rpg.dao.{PersoDao, WeaponDao}

import bon.jo.rpg.stat.Perso.WithUI
import bon.jo.rpg.stat.Perso.given
import bon.jo.rpg.stat._
import bon.jo.rpg.resolve.PersoResolveContext._
import bon.jo.rpg.stat.raw.{Perso, Weapon}
import bon.jo.util.Ec
import org.scalajs.dom.document
import org.scalajs.dom.html.{Button, Div}

import scala.collection.mutable.ListBuffer
import bon.jo.rpg.stat.GameId
import bon.jo.rpg.AffectResolver.Resolver
import bon.jo.rpg._

import bon.jo.rpg.resolve.given








trait Rpg extends Ec with ArmesPage with RpgSimu:
  def createButton(addRandomButton: Button): Unit


  val onChangePage = ListBuffer[()=>Unit]()
  val weaponDao: MappedDao[WeaponJS, Weapon] with WeaponDao
  val persoDao: MappedDao[PersoJS, Perso] with PersoDao

  val deckCreation: Div =
 
    $c.div[Div]



  import GameParams.given
  given Timed[GameElement] =bon.jo.rpg.stat.Perso.PeroPero.asInstanceOf[Timed[GameElement]]
  given timeLine: TimeLineOps = TimeLineOps()
  val root = $ref div {
    d =>
      d._class = "container-fluid pt-5"


  }
  document.getElementsByTagName("app-rpg").foreach { e =>
    e.innerHTML = ""
    e += root
  }
  
  
  var cpntMap: Map[GameId.ID,  PerCpnt] = _
  def clearUI(using ui : HtmlUi)=
    ui.choice.clear()
    ui.messageDiv.clear()
    root.appendChild(ui.choice)
    root.appendChild(ui.messageDiv)
  def startRpg =
    given HtmlUi = HtmlUi.Value
    given HtmlRep[Perso, PerCpnt] = HtmlUi.PersoRep
    given HtmlRep[SystemElement, ImuutableHtmlCpnt] = HtmlUi.acctRep
 //   given Resolver[Perso, Perso,Action.Attaque.type] = CalculsPersoPerso

  
    val linkedUI = new WithUI()
    import linkedUI.given
    cpntMap = timeLine.timedObjs.map{ v =>
      val htmlCpnt = v.value.asInstanceOf[Perso].html
      (v.id,htmlCpnt)
    }.toMap
    

    root.style.maxWidth = "80%"
    cpntMap.flatMap(_._2.get).foreach(e => root += e)

    clearUI
    val cpntTimeLine = new TimeLineCpnt( linkedUI)
    root.appendChild(cpntTimeLine.tlView)
    cpntTimeLine.tlView.$userCanDrag()
    cpntTimeLine.doEvent()
    onChangePage += (() => timeLine.stop())





  /*initChoiXperso.$click { _ =>
    persosForGame.map(_.read).foreach(e => {
      e.randomWeapon()
      yl.add(e)
    })
    root.clear()
    startRpg
  }*/

