package bon.jo.app

import bon.jo.app.Types.Pram
import bon.jo.html.DomShell.{ExtendedElement, ExtendedHTMLCollection}
import bon.jo.html.HTMLDef._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.SimpleView.{BsModifier, bsButton, withClose}
import bon.jo.rpg.Action
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.AnyRefBaseStat
import bon.jo.rpg.stat.raw.{Actor, IntBaseStat, Perso, Weapon}
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.{Button, Div, Input, Span}
import org.scalajs.dom.raw.{HTMLElement, HTMLLIElement, HTMLOptionElement, HTMLSelectElement, HTMLUListElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Types {
  type Pram = (Rpg, mutable.ListBuffer[EditStatWithName[Perso]])
}

object EditPersoCpnt extends HtmlRepParam[Perso, Pram, EditStatWithName[Perso]] {


  override def html(memo: Perso, option: Option[Pram]): EditPersoCpnt = {
    new EditPersoCpnt(memo, option)(EditStat)
  }

  implicit val value: HtmlRepParam[Perso, Pram, EditStatWithName[Perso]] = this


}

class EditPersoCpnt(initial: Perso, option: Option[(Rpg, mutable.ListBuffer[EditStatWithName[Perso]])])(repStat: HtmlRep[IntBaseStat, EditStat]) extends EditStatWithName[Perso](initial, option)(repStat) {
  override implicit val rep: HtmlRepParam[Perso, Pram, EditStatWithName[Perso]] = EditPersoCpnt

  override def randomValue: Perso = Actor.randomActor(e => new Perso(initial.id, RandomName(), e))

  val armrG: Button = bsButton("+")

  private var varArmg :Option[Weapon]= None

  armrG.$click { _ =>
    option foreach {
      case (rpg, value) =>
        import rpg.executionContext
        val ul = $c.li[HTMLUListElement]
        rpg.weaponDao.readAll().map {
          e => e.map(w => w -> ($c.li[HTMLLIElement] := {
            li =>
            li.textContent = w.name
              li._class = "list-group-item btn"
              li.$click{
                _ =>
                 varArmg = Some(w)
                  ul.removeFromDom()
              }
          }))
        } foreach  {
          ws =>
            val sel =  ws.toList.map(_._2).foldLeft(ul)(_ += _)
            sel.style.height = "5em"
            sel.style.overflowY = "scroll"
            sel._class = "list-group"
            beforeState(sel)
        }

    }
  }

  override def create(id: Int, name: String, intBaseStat: IntBaseStat, action: List[Action]): Perso = new Perso(id, name, intBaseStat, lvl = 1, action,leftHandWeapon = varArmg)

  override def beforeStatOption: Option[HTMLElement] = Some(armrG)
}
