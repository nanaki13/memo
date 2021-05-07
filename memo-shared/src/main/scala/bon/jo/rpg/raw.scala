package bon.jo.rpg

import bon.jo.rpg

object raw {
  type Action = rpg.Action
  val Action: rpg.Action.type = rpg.Action
  type ActionResolver[A, B] = rpg.ActionResolver[A, B]
  val BattleTimeLine: rpg.BattleTimeLine.type = rpg.BattleTimeLine

  type DoActionTrait[A] = rpg.DoActionTrait[A]
  val DoActionTrait: rpg.DoActionTrait.type = rpg.DoActionTrait
  val State: rpg.State.type = rpg.State
  type State = rpg.State

  type Timed[A] = rpg.Timed[A]
  type TimedTrait[A] = rpg.TimedTrait[A]
  val TimedTrait: rpg.TimedTrait.type = rpg.TimedTrait


}
