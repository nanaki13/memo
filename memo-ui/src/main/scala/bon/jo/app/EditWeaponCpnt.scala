package bon.jo.app

import bon.jo.memo.ui.HtmlRep
import bon.jo.memo.ui.HtmlRep.HtmlRepParam
import bon.jo.rpg.Action
import bon.jo.rpg.stat.raw.{Actor, AnyRefBaseStat, IntBaseStat, Weapon}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object EditWeaponCpnt extends HtmlRepParam[Weapon, mutable.ListBuffer[EditStatWithName[Weapon]], EditStatWithName[Weapon]] {

  override def html(memo: Weapon, option: Option[mutable.ListBuffer[EditStatWithName[Weapon]]]): EditWeaponCpnt = {
    new EditWeaponCpnt(memo, option)(EditStat)
  }

  implicit val value: HtmlRepParam[Weapon, mutable.ListBuffer[EditStatWithName[Weapon]], EditStatWithName[Weapon]] = this


}

class EditWeaponCpnt(initial: Weapon, option: Option[mutable.ListBuffer[EditStatWithName[Weapon]]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends EditStatWithName[Weapon](initial,option)(repStat){
  override implicit val rep: HtmlRepParam[Weapon, ListBuffer[EditStatWithName[Weapon]], EditStatWithName[Weapon]] = EditWeaponCpnt

  override def randomValue: Weapon = new Weapon(RandomName(),1,AnyRefBaseStat[Float](Actor.randomWeaponVal _).map(_.round))

  override def create(name: String, intBaseStat: IntBaseStat, action: List[Action]): Weapon = new Weapon(name,1,intBaseStat,action)
}
