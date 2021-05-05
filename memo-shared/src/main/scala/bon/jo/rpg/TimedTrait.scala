package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx

object TimedTrait {
  implicit class TimedObject[A: Timed](val value: A) extends TimedTrait[A] {
    override val workerTimed: Timed[A] = implicitly[Timed[A]]


    override def timed: TimedObject.this.type = this
  }
}

trait TimedTrait[A] {
  def value: A

  val workerTimed: Timed[A]

  def speed: Int = workerTimed.speed(value)

  def canChoice: List[Action]= workerTimed.canChoice(value)

  def action_=(action: ActionCtx): Unit = workerTimed.action_=(value, action)

  def action: ActionCtx = workerTimed.action(value)

  def timed: this.type

  def pos: Int = workerTimed.pos(value)

  def pos_=(a: Int): Unit = workerTimed.pos_=(value, a)

  def simpleName: String = workerTimed.simpleName(value)

  override def toString: String = value.toString
}