package bon.jo.rpg

import bon.jo.rpg.CommandeCtx
import bon.jo.rpg.stat.GameElement
import bon.jo.rpg.stat.GameId
import bon.jo.rpg.stat.GameId.ID.given
object TimedTrait:
  private var id = 0

  
  given Ordering[TimedTrait[_]]  = (a,b) => GameId.ID.safe(a.id).compareTo(GameId.ID.safe(b.id))
  private def getId: GameId.ID =
    id += 1
    GameId.ID(id)
  case class TimedObject( value: GameElement,id : GameId.ID,_pos : Int= 0,_commandeCtx : CommandeCtx)(using Timed[GameElement]) extends TimedTrait[GameElement]:
    override val workerTimed: Timed[GameElement] = summon[Timed[GameElement]]
    override def withPos(i: Int): TimedTrait[GameElement] = copy(_pos = i)


    override def withCommandeCtx(i: CommandeCtx): TimedTrait[GameElement] = copy(_commandeCtx = i)
    def withValue(a: GameElement): TimedTrait[GameElement]  = copy(value = a)
  extension (value: GameElement)(using Timed[GameElement]) 
    def timed =  TimedObject(value,id   = getId,0,CommandeCtx.Rien)

trait TimedTrait[A] {
  val id : GameId.ID
  val _pos : Int
  val _commandeCtx : CommandeCtx
  val value: A
  def withPos(i: Int): TimedTrait[A]
  def withValue(a: A): TimedTrait[A]
 

  val workerTimed: Timed[A]

  def cast[T ] = this.asInstanceOf[T]

  def speed: Int = workerTimed.speed(value)

  def canChoice: List[Commande]= workerTimed.canChoice(value)
  def withCommandeCtx(i: CommandeCtx): TimedTrait[A]


  def commandeCtx: CommandeCtx = _commandeCtx

 // def timed: TimedTrait[A]

  def pos: Int = _pos

  //def pos_=(a: Int): Unit = workerTimed.pos_=(value, a)



  def simpleName: String = workerTimed.simpleName(value)

  override def toString: String = value.toString
}