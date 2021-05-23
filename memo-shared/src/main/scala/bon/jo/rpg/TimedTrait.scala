package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.stat.GameElement

object TimedTrait:
  private var id = 0
  def getId: Int =
    id += 1
    id
  case class TimedObject( value: GameElement,id : Int,_pos : Int= 0,_action : ActionCtx[GameElement])(using Timed[GameElement]) extends TimedTrait[GameElement]:
    override val workerTimed: Timed[GameElement] = summon[Timed[GameElement]]
    override def withPos(i: Int): TimedTrait[GameElement] = copy(_pos = i)

   // override def timed:TimedTrait[GameElement] = this

    override def withAction(i: ActionCtx[GameElement]): TimedTrait[GameElement] = copy(_action = i)
    def withValue(a: GameElement): TimedTrait[GameElement]  = copy(value = a)
  extension (value: GameElement)(using Timed[GameElement]) 
    def timed =  TimedObject(value,id   = getId,0,ActionCtx.Rien)

trait TimedTrait[A] {
  val id : Int
  val _pos : Int
  val _action : ActionCtx[A]
  val value: A
  def withPos(i: Int): TimedTrait[A]
  def withValue(a: A): TimedTrait[A]
 

  val workerTimed: Timed[A]

  def speed: Int = workerTimed.speed(value)

  def canChoice: List[Action]= workerTimed.canChoice(value)
  def withAction(i: ActionCtx[A]): TimedTrait[A]


  def action: ActionCtx[A] = _action

 // def timed: TimedTrait[A]

  def pos: Int = _pos

  //def pos_=(a: Int): Unit = workerTimed.pos_=(value, a)



  def simpleName: String = workerTimed.simpleName(value)

  override def toString: String = value.toString
}