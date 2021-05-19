package bon.jo.app


import bon.jo.rpg.Action
import bon.jo.rpg.stat.Actor.Id
import bon.jo.rpg.stat.raw.{Actor, AnyRefBaseStat, IntBaseStat, Weapon}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import SType._
import bon.jo.dao.Dao
import bon.jo.html.HtmlRep
import bon.jo.html.HtmlRep.HtmlRepParam

object EditWeaponCpnt extends HtmlRepParam[Weapon, SType.Param[Weapon], EditStatWithName[Weapon]] {

  override def html(memo: Weapon, option: Option[SType.Param[Weapon]]): EditWeaponCpnt = {
    new EditWeaponCpnt(memo, option)(EditStat)
  }


  object Implicit{
    type Hrep = HtmlRepParam[Weapon, SType.Param[Weapon], EditStatWithName[Weapon]]
    implicit val value: Hrep = EditWeaponCpnt
  }


}

class EditWeaponCpnt(initial: Weapon, option: Option[SType.Param[Weapon]])(repStat: HtmlRep[IntBaseStat, EditStat]) extends EditStatWithName[Weapon](initial,option)(repStat){
  override implicit val rep: HtmlRepParam[Weapon, SType.Param[Weapon], EditStatWithName[Weapon]] = EditWeaponCpnt

  override def randomValue: Weapon = new Weapon(initial.id,RandomName(),1,AnyRefBaseStat[Float](Actor.randomWeaponVal _).map(_.round))

  override def create(id : Int,name: String, intBaseStat: IntBaseStat, action: List[Action]): Weapon = new Weapon(id,name,1,intBaseStat,action)

  override val dao: Dao[Weapon, Int] = option.rpg.weaponDao
}
