package bon.jo.rpg

import bon.jo.rpg.Action.{ActionCtx, readCibleRec}
import bon.jo.rpg.Action.ActionCtx.ActionCibled
import bon.jo.rpg.StdinUtil.fromStdin

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


  trait PlayerUI extends PlayerMessage{

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
    implicit object value extends PlayerUI {
      type T = MessagePlayer
      override def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = fromStdIn(d, cible)

      override def message(str: String,timeToDisplay : Int): Unit = println(str)

      override def message(str: String): MessagePlayer = new MessagePlayer {}

      override def clear(str: MessagePlayer): Unit = {

      }
    }
  }
  def fromStdIn(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = {
    println(s"choisir action de ${d.simpleName}")
    Future.successful( fromStdIn match {
      case AttaqueMainGauche => AttaqueMainGauche.fromStdIn(cible)
      case AttaqueMainDroite => AttaqueMainDroite.fromStdIn(cible)
      case Defendre => ActionCtx.Defendre
      case Rien => ActionCtx.Rien
      case _ => ActionCtx.Rien
    })
  }

  def unapply(str: String): Option[Action] = {
    str match {
      case Action.AttaqueMainGauche.name => Some(AttaqueMainGauche)
      case Action.AttaqueMainDroite.name => Some(AttaqueMainDroite)
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

  case object AttaqueMainGauche extends Action

  case object AttaqueMainDroite extends Action

  case object Defendre extends Action

  case object Rien extends Action

  val values : List[Action] = List(AttaqueMainDroite, AttaqueMainGauche, Defendre, Rien)


}