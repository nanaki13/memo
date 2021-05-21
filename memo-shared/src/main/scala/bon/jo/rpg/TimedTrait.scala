package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx

object TimedTrait {
  private var id = 0
  def getId: Int = {
    id += 1
    id
  }
  case class TimedObject[A: Timed]( value: A,id : Int,_pos : Int= 0,_action : ActionCtx) extends TimedTrait[A] {
    override val workerTimed: Timed[A] = implicitly[Timed[A]]
    override def withPos(i: Int): TimedTrait[A] = copy(_pos = i)

    override def timed: TimedObject.this.type = this

    override def withAction(i: ActionCtx): TimedTrait[A] = copy(_action = i)
  }
  implicit class Base[A: Timed](value: A) extends TimedObject[A](value,id   = getId,0,ActionCtx.Rien)
}

trait TimedTrait[A] {
  val id : Int
  val _pos : Int
  val _action : ActionCtx
  def withPos(i: Int): TimedTrait[A]

  def value: A

  val workerTimed: Timed[A]

  def speed: Int = workerTimed.speed(value)

  def canChoice: List[Action]= workerTimed.canChoice(value)
  def withAction(i: ActionCtx): TimedTrait[A]


  def action: ActionCtx = _action

  def timed: this.type

  def pos: Int = _pos

  //def pos_=(a: Int): Unit = workerTimed.pos_=(value, a)



  def simpleName: String = workerTimed.simpleName(value)

  override def toString: String = value.toString
}