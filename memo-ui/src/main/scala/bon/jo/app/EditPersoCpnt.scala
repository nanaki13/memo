package bon.jo.app

import bon.jo.app.Experimental.{StringToHtml, t}
import bon.jo.app.SType.ExParam
import bon.jo.app.Types.Pram
import bon.jo.dao.Dao
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HTMLDef._
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.HtmlRepParam
import bon.jo.memo.ui.SimpleView.bsButton
import bon.jo.rpg.Action
import bon.jo.rpg.stat.raw.{Actor, IntBaseStat, Perso, Weapon}
import org.scalajs.dom.html.{Button, TextArea}
import org.scalajs.dom.raw.{HTMLElement, HTMLLIElement, HTMLUListElement}

import scala.collection.mutable

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

  override def randomValue: Perso = Actor.randomActor(e => new Perso(initial.id, RandomName(),"Le plus beau des héros", e))
  override val dao: Dao[Perso, Int] = option.rpg.persoDao
  val equipRight: Button = bsButton("+")
  val equipLeft: Button = bsButton("+")
  private var varRightHand: Option[Weapon] = initial.rightHandWeapon
  private var varLeftHand: Option[Weapon] = initial.leftHandWeapon

  def equipAction(addButton: Button, updateTitle: HTMLElement)(optionF: Option[Weapon] => Unit) = {
    addButton.$click { _ =>
      option foreach {
        case (rpg, value) =>
          import rpg.executionContext
          val ul = $c.ul[HTMLUListElement]
          rpg.weaponDao.readAll().map {
            e =>
              e.map(w => w -> ($c.li[HTMLLIElement] := {
                li =>
                  li.textContent = w.name
                  li._class = "list-group-item btn"
                  li.$click {
                    _ =>
                      val s = Some(w)
                      optionF(s)
                      updateTitle.textContent = txt(s)
                      ul.removeFromDom()
                  }
              }))
          } foreach {
            ws =>
              val sel = ws.toList.map(_._2).foldLeft(ul)(_ += _)
              sel.style.height = "5em"
              sel.style.overflowY = "scroll"
              sel._class = "list-group"
              beforeState(sel)
          }

      }
    }
  }


  def txt(optionW: Option[Weapon]): String = optionW.map(_.name).getOrElse("-")

  def spanArm(optionW: Option[Weapon]): HTMLElement = $ref span (_.textContent = txt(optionW))

  private val leftArm = spanArm(initial.leftHandWeapon)
  private val rightArm = spanArm(initial.rightHandWeapon)
  private val handsCont = $va div(leftArm, equipLeft, rightArm, equipRight)
  private val descTa = initial.desc.tagTyped[TextArea](t.textarea)
  override def create(id: Int, name: String, intBaseStat: IntBaseStat, action: List[Action]): Perso =
    new Perso(id, name,descTa.value, intBaseStat, lvl = 1, action, leftHandWeapon = varLeftHand, rightHandWeapon = varRightHand)

  override def beforeStatOption: Option[HTMLElement] = Some( $va div(descTa,handsCont))


  equipAction(equipRight, rightArm) {
    r =>
      varRightHand = r
      varRightHand foreach (_ => updateAction(read))
  }
  equipAction(equipLeft, leftArm){
    r =>
      varLeftHand = r
      varLeftHand foreach  ( _ => updateAction(read))
  }
}
