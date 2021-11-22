package bon.jo.app

import bon.jo.rpg.ui.Export
import bon.jo.rpg.ui.Export.{PersoJS, WeaponJS}
import bon.jo.dao.IndexedDB
import bon.jo.dao.IndexedDB.DBExeception
import bon.jo.dao.LocalJsDao.{MappedDao}
import bon.jo.dao.LocalJsDao.given
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.{PopUp, SimpleView}
import bon.jo.rpg.dao.PersoDao.PersoDaoJs
import bon.jo.rpg.dao.IntMappedDao
import bon.jo.rpg.dao.{PersoDao, WeaponDao}
import bon.jo.rpg.dao.WeaponDao.WeaponDaoJs
import bon.jo.rpg.stat.raw.{Perso, Weapon}
import org.scalajs.dom.{console, document}
import org.scalajs.dom.html.{Anchor, Button, Div, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLLIElement, HTMLUListElement}

import java.nio.charset.Charset
import java.util.Base64
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.JSON
import scala.util.{Failure, Success}
import bon.jo.rpg.dao.FormuleDao.FormuleDaoJs
import bon.jo.rpg.dao.FormuleJs
import bon.jo.rpg.resolve.Formule
import bon.jo.rpg.dao.FormuleDao
import bon.jo.rpg.Affect
import bon.jo.rpg.resolve.FormuleType
import bon.jo.rpg.ui.edit.EditFormauleAffect
import bon.jo.rpg.ui.page.EditFormulePage
import bon.jo.rpg.ui.page.ChangeLog
import bon.jo.rpg.ui.Rpg

object RpgJsMain extends App:
  given  ((Weapon, Int) => Weapon) = _.withId(_) 
  given  ((Perso, Int) => Perso) = _.withId(_) 
  document.body.classList.add("bg-1")
  object editPage extends EditFormulePage
  given Rpg with
    override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
    val weaponJsDao: WeaponDaoJs = new WeaponDaoJs {
      override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
      def keyPath : String = "id"
    }
    val persoJsDao: PersoDaoJs = new PersoDaoJs {
      override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
      def keyPath : String = "id"
    }
    given Conversion[(Affect,FormuleType),scalajs.js.Any] = s => 
      println(js.Array(s._1.id,s._2.toString))
      js.Array(s._1.id,s._2.toString)
    val formuleJsDao: FormuleDaoJs = new FormuleDaoJs {
      override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
      def keyPath : Array[String] = Array("affect","formuleType")
    }
    
    override val weaponDao: MappedDao[WeaponJS, Weapon,Int] with WeaponDao with IntMappedDao[WeaponJS, Weapon]= WeaponDao(weaponJsDao)
    override val persoDao: MappedDao[PersoJS, Perso,Int] with PersoDao = PersoDao(persoJsDao)
    override val formuleDao: MappedDao[FormuleJs, Formule,(Affect,FormuleType)] with FormuleDao = FormuleDao(formuleJsDao)

    private val fromChild =  $c.a[Anchor]:= (menuLink => {menuLink._class = "nav-item menu-link"})
    override def createButton(addRandomButton: Button): Unit =
      fromChild.clear()
      fromChild += addRandomButton
      menu.cont += fromChild

    def exportF() : Unit =
      weaponDao.readAll().zip(persoDao.readAll()) map {
         case (ws, perso) => js.Dynamic.literal(
           w = ws.map(_.copy(id = 0)).map(weaponDao.mapper.map).toJSArray,
           p = perso.map(_.copy(id = 0)).map(persoDao.mapper.map).toJSArray
         )
       } onComplete{
         case Success(value) =>
           val exportV = Base64.getEncoder.encodeToString( JSON.stringify(value).getBytes("utf-8"))
           val popUpCotnet : TextArea = $c.textarea[TextArea] := {
             (r : TextArea) =>
               r.value=exportV
           }
           PopUp(popUpCotnet)
         case Failure(exception) =>
       }
    def importData(str : String) : Unit =
      weaponDao.initId zip persoDao.initId flatMap  {
        _ =>
          val data = JSON.parse(new String(Base64.getDecoder.decode(str), "utf-8"))
          val wJs: js.Array[Weapon] = data.w.asInstanceOf[js.Array[WeaponJS]].flatMap(WeaponJS.unapply)
          val pJs: js.Array[Perso] = data.p.asInstanceOf[js.Array[PersoJS]].flatMap(PersoJS.unapply)

          Future.sequence(wJs.map(weaponDao.createOrUpdate).toSeq ++ pJs.map(persoDao.createOrUpdate).toSeq)
      } onComplete{
        case Failure(exception) => PopUp("Import KO")
        case Success(value) => PopUp("Import OK")
      }



    def importDataPopUp() : Unit = 
      val ta : TextArea = $c.textarea[TextArea]
      val impBtn : Button =  SimpleView.bsButton("import")
      val div = $va div List(ta,impBtn)
      impBtn.$click{_=>
        try
          importData(ta.value)
        catch
          case (e : Exception) => PopUp("Donner invalid")

      }
      PopUp(div)

    
    val menu = new Menu(
      "éditer/créer Arme" -> initChoixArme,

      "éditer/créer Perso" -> initChoixPerso,
      "Simulation" -> simulation,
      "Export" -> exportF,"Import" -> importDataPopUp,
      "Test Formule" -> editPage.editPage(using root),
      "Edit Formule" -> (() => EditFormauleAffect.simulation),
      "News" -> (() =>
        root.clear()
        root += ChangeLog.head
        ))
    def init(): HTMLElement =
      root.parentElement += menu.cont


  class Menu(val menuItems: (String, () => Unit)*)(using Rpg : Rpg):
    val links: Seq[HTMLElement] = menuItems.map {
      case (str, unit) =>
        $c.a[Anchor] := (menuLink => {
          menuLink.text = str
          menuLink._class = "dropdown-item nav-link menu-link"
          menuLink.$click { _ =>
            Rpg.root.clear()
            Rpg.onChangePage.foreach(_())
            Rpg.onChangePage.clear()
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

    private def aSubMenu(t: String) = $c.a[Anchor] := {
      s =>
        s._class = "nav-link dropdown-toggle"
        s.href = "#"
        s.text = t
        s.$attr(List("data-toggle" -> "dropdown", "role" -> "button", "aria-haspopup" -> "true", "aria-expanded" -> "false"))
    }



  type RpgUsing = Rpg ?=> Unit
  def rpg(using Rpg) :  Rpg  = summon
  def init(): RpgUsing=
    val rp = rpg
    import rp.executionContext
    given HTMLElement  = rpg.root
    IndexedDB.init(rp.weaponDao.daoJs, rp.persoDao.daoJs, rp.formuleDao.daoJs) map { _ =>
      rp.init()
    } onComplete{
      case Failure(exception) => exception match
        case ex@DBExeception(e) => ex.printStackTrace();console.log(e)
        case e => e.printStackTrace()
      case Success(_) =>     PopUp("start ok");  EditFormauleAffect.simulation
    }
  init()
  














