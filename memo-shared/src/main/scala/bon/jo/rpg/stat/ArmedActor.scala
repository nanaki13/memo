package bon.jo.rpg.stat

import bon.jo.rpg.Action
import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.stat.Actor.{ActorBaseStats, WeaponBaseState}
import bon.jo.rpg.stat.BaseState.ImplicitCommon._

trait ArmedActor {
  self: ActorBaseStats =>
  var leftHand: Option[WeaponBaseState]
  var rightHand: Option[WeaponBaseState]

  def twoHand = leftHand == rightHand

  def leftArmedStat(): GenBaseState[Float] = {
    this growPercent leftHand.getOrElse(BaseState.`0`)
  }

  def rightArmedStat(): GenBaseState[Float] = {
    this growPercent rightHand.getOrElse(BaseState.`0`)
  }

  def twoAndStat(): GenBaseState[Float] = this growPercent (rightHand.getOrElse(BaseState.`0`) + leftHand.getOrElse(BaseState.`0`))

  def action: List[Action] = leftHand.map(_.action) flatMap {
    e =>
      rightHand.map(_.action ++ e)
  } getOrElse (Nil)

  var actionCtx: Option[ActionCtx] = None
}
