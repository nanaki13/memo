package bon.jo.rpg

import bon.jo.rpg.Action.{ActionCtx, readCibleRec}
import bon.jo.rpg.Action.ActionCtx.ActionCibled
import bon.jo.rpg.Action.PlayerUIStdIn.value
import bon.jo.rpg.StdinUtil.fromStdin
import bon.jo.rpg.stat.Perso
import bon.jo.rpg.stat.Perso.{PersoOps, PlayerPersoUI}
import bon.jo.ui.UpdatableCpnt

import scala.concurrent.Future

sealed trait Action extends Product {
  val name = toString

  def fromStdIn(cible: List[TimedTrait[_]]): ActionCtx = {
    new ActionCibled(this, readCibleRec(cible))
  }

}

object Action {
  trait ActionCtx {
    def action: Action

    def cible: List[TimedTrait[_]]
  }

  object ActionCtx {
    case object Defendre extends ActionWithoutCible(Action.Defendre)

    case object Rien extends ActionWithoutCible(Action.Rien)

    class ActionWithoutCible(val action: Action) extends ActionWithoutCibleOps

    class ActionCibled(val action: Action, val cible: List[TimedTrait[_]]) extends ActionCtx
  }

  trait ActionWithoutCibleOps extends ActionCtx {
    override def cible: List[TimedTrait[_]] = Nil
  }

  object PlayerUI{


  }

  trait PlayerUI extends PlayerMessage{

    type S
    def cpntMap : S => UpdatableCpnt[S]
    def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx]

  }
  trait PlayerMessage {
    type T <: MessagePlayer
    def message(str : String,timeToDisplay : Int) : Unit
    def message(str : String) : MessagePlayer
    def clear(str : T) : Unit
  }

  trait MessagePlayer
  object PlayerUIStdIn{
    object Value extends PlayerUI {
      type T = MessagePlayer
      override def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = fromStdIn(d, cible)

      override def message(str: String,timeToDisplay : Int): Unit = println(str)

      override def message(str: String): MessagePlayer = new MessagePlayer {}

      override def clear(str: MessagePlayer): Unit = {

      }

      override type S = Any

      override def cpntMap: Any => UpdatableCpnt[Any] = e => new UpdatableCpnt[Any] {
        override def update(value: Option[Any]): Unit = {
          println(value)
        }
      }
    }
    implicit val value : PlayerUI = Value
  }
  def fromStdIn(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = {
    println(s"choisir action de ${d.simpleName}")
    Future.successful( fromStdIn match {
      case  Attaque.MainGauche =>  Attaque.MainGauche.fromStdIn(cible)
      case Attaque.MainDroite => Attaque.MainDroite.fromStdIn(cible)
      case Defendre => ActionCtx.Defendre
      case Rien => ActionCtx.Rien
      case _ => ActionCtx.Rien
    })
  }

  def unapply(str: String): Option[Action] = {
    str match {
      case Attaque.MainGauche.name => Some( Attaque.MainGauche)
      case Attaque.MainDroite.name => Some(Attaque.MainDroite)
      case Action.Rien.name => Some(Rien)
      case _ => None
    }
  }


  def fromStdIn: Action = {
    fromStdin(Action.values)
  }


  def readCibleRec(cible: List[TimedTrait[_]]): List[TimedTrait[_]] = {
    def f(t: TimedTrait[_]) = t.simpleName

    List(fromStdin(cible, f))
  }

  case object Attaque extends Action{
    case object MainDroite extends Action
    case object MainGauche extends Action
  }
  case object Soin extends Action
  case object Defendre extends Action

  case object Rien extends Action

  val values : List[Action] = List(Attaque.MainDroite, Attaque.MainGauche, Defendre, Rien)


}