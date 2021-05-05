package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx

trait Timed[A] {
  def simpleName(value: A): String


  def speed(a: A): Int

  def action_=(a: A, action: ActionCtx): Unit

  def action(a: A): ActionCtx

  def pos(a: A): Int

  def pos_=(a: A, pos: Int): Unit

  def canChoice(a : A):List[Action]
}
