package bon.jo.app

import bon.jo.app.Export.StatJS
import bon.jo.rpg.Action
import bon.jo.rpg.stat.Actor.{Lvl, Weapon, WeaponBaseState}
import bon.jo.rpg.stat.StatsWithName
import bon.jo.rpg.stat.raw.{AnyRefBaseStat, BaseState, IntBaseStat, Perso, Weapon}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Export {

  def apply(e: StatsWithName with Lvl): js.Object with js.Dynamic = {
    js.Dynamic.literal(
      name = e.name,
      id = e.id,
      lvl = e.lvl,
      stats = StatJS(e.stats),
      action = e.action.map(_.name).toJSArray
    )
  }

  object PersoJS  {
   // type R = PersoJS

     def apply(t: Perso): PersoJS = {
      val dPerso = Export.apply(t)
      dPerso.leftHandWeapon = t.leftHandWeapon.map(WeaponJS(_)).orUndefined
      dPerso.rightHandWeapon = t.rightHandWeapon.map(WeaponJS(_)).orUndefined
      dPerso.asInstanceOf[PersoJS]
    }

     def unapply(value: PersoJS): Option[Perso] = {
      value.stats match {
        case StatJS(stat) =>
          val action = value.action flatMap Action.unapply
          val left = value.leftHandWeapon.toOption.flatMap(WeaponJS.unapply)
          val right = value.leftHandWeapon.toOption.flatMap(WeaponJS.unapply)
          Some(new Perso(value.name, stat, value.lvl, action, left, right, value.id))
        case _ => None
      }

    }
  }

  object WeaponJS {



    def unapply(weaponJS: WeaponJS): Option[WeaponBaseState] = {
      weaponJS.stats match {
        case StatJS(stat) =>
          val action = weaponJS.action flatMap Action.unapply
          Some(Weapon(weaponJS.name, weaponJS.lvl, stat, action, weaponJS.id))
        case _ => None
      }

    }

    def apply(weapon: WeaponBaseState): WeaponJS = {
      Export.apply(weapon: StatsWithName with Lvl).asInstanceOf[WeaponJS]
    }
  }

  trait NameIdStat extends js.Object {
    val lvl: Int
    val name: String
    val id: Int
    val stats: StatJS
    val action: List[String]
  }

  trait JSCompanion[T] {
    type R

    def unapply(t: R): Option[T]

    def apply(R: T): R
  }

  object StatJS  {


    def apply(stats: IntBaseStat): StatJS = {
      js.Dynamic.literal(
        hp = stats.hp,
        sp = stats.sp,
        viv = stats.viv,
        vit = stats.vit,
        chc = stats.chc,
        mag = stats.mag,
        res = stats.res,
        psy = stats.psy,
        str = stats.str,
      ).asInstanceOf[StatJS]
    }

    def unapply(value: StatJS): Option[IntBaseStat] = {
      Some(AnyRefBaseStat[Int](value.hp,
        value.sp,
        value.viv,
        value.str,
        value.mag,
        value.vit,
        value.psy,
        value.res,
        value.chc))
    }
  }

  trait StatJS extends js.Object {
    val hp: Int
    val sp: Int
    val viv: Int
    val str: Int
    val mag: Int
    val vit: Int
    val psy: Int
    val res: Int
    val chc: Int

  }

  trait PersoJS extends NameIdStat {
    val leftHandWeapon: js.UndefOr[WeaponJS]
    val rightHandWeapon: js.UndefOr[WeaponJS]
  }

  trait WeaponJS extends NameIdStat
}
