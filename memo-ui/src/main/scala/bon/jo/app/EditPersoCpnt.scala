package bon.jo.app

import bon.jo.html.DomShell.ExtendedHTMLCollection
import bon.jo.html.HTMLDef.{$c, $l, $ref, $t, HtmlOps}
import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.ui.{HtmlRep, SimpleView}
import bon.jo.memo.ui.HtmlRep.{HtmlRepParam, PrXmlId}
import bon.jo.memo.ui.SimpleView.{BsModifier, withClose}
import bon.jo.rpg.Action
import bon.jo.rpg.stat.AnyRefBaseStat
import bon.jo.rpg.stat.raw.{Actor, IntBaseStat, Perso}
import bon.jo.ui.{ReadableCpnt, UpdatableCpnt}
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object EditPersoCpnt extends HtmlRepParam[Perso, mutable.ListBuffer[EditStatWithName[Perso]], EditStatWithName[Perso]] {

  override def html(memo: Perso, option: Option[mutable.ListBuffer[EditStatWithName[Perso]]]): EditPersoCpnt = {
    new EditPersoCpnt(memo, option)(EditStat)
  }

  implicit val value: HtmlRepParam[Perso, mutable.ListBuffer[EditStatWithName[Perso]], EditStatWithName[Perso]] = this


}

class EditPersoCpnt(initial: Perso, option: Option[mutable.ListBuffer[EditStatWithName[Perso]]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends EditStatWithName[Perso](initial,option)(repStat){
  override implicit val rep: HtmlRepParam[Perso, ListBuffer[EditStatWithName[Perso]], EditStatWithName[Perso]] = EditPersoCpnt

  override def randomValue: Perso = Actor.randomActor(e => new Perso(RandomName(),e))

  override def create(name: String, intBaseStat: IntBaseStat, action: List[Action]): Perso = new Perso(name,intBaseStat,lvl = 1,action)
}
