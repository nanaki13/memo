package bon.jo.app

import bon.jo.app.EditWeaponCpnt.Implicit.Hrep
import bon.jo.app.Export.WeaponJS
import bon.jo.html.HTMLDef.{$c, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.HtmlRep.PrXmlId
import bon.jo.memo.ui.SimpleView
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.raw.{Actor, Weapon}
import org.scalajs.dom.console
import org.scalajs.dom.html.Div

import scala.collection.mutable
import scala.util.{Failure, Success}

trait ArmesPage {
  self : Rpg =>
  val weaponForGame = mutable.ListBuffer.empty[EditStatWithName[Weapon]]
  val deckCreation : Div = $c.div[Div]
  def initChoixArme() = {
    implicit val v: Hrep = EditWeaponCpnt.Implicit.value
    WeaponDao.readIds().map {
      case Nil => List(0)
      case e => println(e);e
    }.map(_.max).map(Id.init[Weapon](_)).map {
      _ =>

        WeaponDao.readAll().onComplete {

          case Failure(exception) => console.log(exception)
          case Success(value) => {

            value.flatMap(WeaponJS.unapply).foreach((w : Weapon) => {
              val htmlCpnt = w.htmlp(weaponForGame)
              weaponForGame += htmlCpnt
              deckCreation ++= htmlCpnt.list
            })


          }
        }
        val p = Actor.randomWeapon()
        val persoCpnt = p.htmlp(weaponForGame)
        weaponForGame += persoCpnt
        deckCreation ++= persoCpnt.list
        root ++= deckCreation
        val saveB = SimpleView.bsButton("save")

        saveB.$click { _ =>
          weaponForGame.map(e => (e, e.read)).map {
            case (view, v) =>
              if (v.id == 0) {
                (view, v.copy(id = Id[Weapon]), WeaponDao.create _)
              } else {
                (view, v, WeaponDao.update(_, None))
              }: (EditStatWithName[Weapon], Weapon, WeaponJS => WeaponDao.FO)
          }.map {
            case (view, w, fw) => (view, Export.WeaponJS(w), fw)
          }.map { case (view, w, fw) => (view, fw(w)) }.foreach {
            case (view, e) =>
              e.onComplete {
                case Failure(exception) => throw exception
                case Success(value) => {   view.update(value.flatMap(WeaponJS.unapply))


                }
              }
          }
        }

        root += saveB


    } onComplete {
      case Failure(exception) => {
        scalajs.js.special.debugger()
        console.log(exception)
      }
      case Success(value) =>
    }

  }

}
