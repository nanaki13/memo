package bon.jo.app


import bon.jo.app.Export.WeaponJS
import bon.jo.dao.IndexedDB
import bon.jo.dao.IndexedDB.DBExeception
import bon.jo.dao.LocalJsDao.MappedDao
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.PopUp
import bon.jo.rpg.dao.PersoDao.PersoDaoJs
import bon.jo.rpg.dao.{PersoDao, WeaponDao}
import bon.jo.rpg.dao.WeaponDao.WeaponDaoJs
import bon.jo.rpg.stat.raw.{Perso, Weapon}
import org.scalajs.dom.console
import org.scalajs.dom.html.{Anchor, Button, Div}
import org.scalajs.dom.raw.{HTMLElement, HTMLLIElement, HTMLUListElement}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


object AppLoaderExample extends App {
  object Rpg extends Rpg {
    override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
    val jsDao = new WeaponDaoJs {
      override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
    }
    val jsPersoDao = new PersoDaoJs {
      override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
    }

    override val weaponDao: MappedDao[WeaponJS, Weapon] with WeaponDao = WeaponDao(jsDao)
    override val persoDao: MappedDao[Export.PersoJS, Perso] with PersoDao = PersoDao(jsPersoDao)

    val fromChild =  $c.a[Anchor]:= (menuLink => {menuLink._class = "nav-item menu-link"})
    override def createButton(addRandomButton: Button): Unit = {
      fromChild.clear()
      fromChild += addRandomButton
      menu.cont += fromChild
    }
    val menu = new Menu(
      "éditer/créer Arme" -> initChoixArme,
      "éditer/créer Perso" -> initChoixPerso)

    def init() = {
      root.parentElement += menu.cont
    }


  }
  class Menu(val menuItems: (String, () => Unit)*) {
    val links: Seq[HTMLElement] = menuItems.map {
      case (str, unit) =>
        $c.a[Anchor] := (menuLink => {
          menuLink.text = str
          menuLink._class = "dropdown-item nav-link menu-link"
          menuLink.$click { _ =>
            Rpg.root.clear()

            unit()
          }
        })

    }
    val cont: HTMLElement = $ref nav {
      d =>
        d._class = "menu nav bg-white rounded"
        val li = $c.li[HTMLLIElement] := (_._class = "nav-item dropdown")
        li += aSubMenu("Menu")
        li += ($c.div[Div] := {
          e =>
            e._class = "dropdown-menu"
            e ++= links.toList
        })
        d += li


    }
    // <a class="nav-link dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">Dropdown</a>
    def aSubMenu(t: String) = $c.a[Anchor] := {
      s =>
        s._class = "nav-link dropdown-toggle"
        s.href = "#"
        s.text = t
        s.$attr("data-toggle" -> "dropdown", "role" -> "button", "aria-haspopup" -> "true", "aria-expanded" -> "false")
    }



  }

  import Rpg.executionContext
  Rpg.init()
  IndexedDB.init(Rpg.jsDao.name, Rpg.jsPersoDao.name) map { _ =>


    Rpg


  } onComplete{
    case Failure(exception) => exception match {
      case ex@DBExeception(e) => ex.printStackTrace();console.log(e)
      case e => e.printStackTrace()
    }
    case Success(_) =>     PopUp("start ok")
  }
}















