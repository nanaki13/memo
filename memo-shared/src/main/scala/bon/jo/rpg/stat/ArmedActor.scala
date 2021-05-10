package bon.jo.rpg.stat

import bon.jo.rpg.Action
import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.stat.Actor.{ActorBaseStats, WeaponBaseState}
import bon.jo.rpg.stat.AnyRefBaseStat.r
import bon.jo.rpg.stat.BaseState.ImplicitCommon._

trait ArmedActor {
  self: ActorBaseStats =>
  var leftHand: Option[WeaponBaseState]
  var rightHand: Option[WeaponBaseState]

  def twoHand = leftHand == rightHand

  def leftArmedStat(): AnyRefBaseStat[Float] = {
    this growPercent leftHand.getOrElse(BaseState.`0`)
  }

  def rightArmedStat(): AnyRefBaseStat[Float] = {
    this growPercent rightHand.getOrElse(BaseState.`0`)
  }

  def twoAndStat(): AnyRefBaseStat[Float] = this growPercent (rightHand.getOrElse(BaseState.`0`) + leftHand.getOrElse(BaseState.`0`))

  def mapAttaque(ret: Action): Action => Action ={
    case  Action.Attaque => ret
    case a : Action => a
  }
  def leftHandAction = leftHand.map(e=>e.action.map(mapAttaque(Action.Attaque.MainGauche))).getOrElse(Nil)
  def rightHandAction = rightHand.map(e=>e.action.map(mapAttaque(Action.Attaque.MainDroite))).getOrElse(Nil)
  var action: List[Action] = Nil

  var actionCtx: Option[ActionCtx] = None

  def randomSoin(weapon: Actor.Weapon): WeaponBaseState = {
    if(r.nextDouble()>0.5){
      weapon.action = weapon.action :+ Action.Soin
    }

    weapon
  }

  def randomWeapon() = {
    leftHand = Some(randomSoin(Actor.randomWeapon()))
    rightHand = Some(randomSoin(Actor.randomWeapon()))
  }
}
