package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx.ActionCibled
import bon.jo.rpg.Action.{ActionCtx, readCibleRec}
import bon.jo.rpg.StdinUtil.fromStdin

import scala.concurrent.Future

sealed trait Action extends Product {
  val name = toString

  def fromStdIn(cible: List[TimedTrait[_]]): ActionCtx = {
    new ActionCibled(this, readCibleRec(cible))
  }

}

object Action {


  case object Attaque extends Action {
    case object MainDroite extends Action

    case object MainGauche extends Action
  }

  case object Soin extends Action
  case object Aoe extends Action
  case object Garde extends Action
  case object Evasion extends Action
  case object Voler extends Action
  case object  ChangerDequipement extends Action
  case object Talent
  case object Rien extends Action

  def applyFrom(from : List[Action])(string: String) : Option[Action] = {
    println(string)
    from.map(e=> {
      println(e)
      e
    }).find(_.name == string)
  }
  val commonValues: List[Action] = List(Attaque.MainDroite, Attaque.MainGauche,Voler , Garde,Evasion, Rien)
  trait ActionCtx {
    def action: Action

    def cible: List[TimedTrait[_]]
  }

  object ActionCtx {
    case object Garde extends ActionWithoutCible(Action.Garde)

    case object Rien extends ActionWithoutCible(Action.Rien)

    class ActionWithoutCible(val action: Action) extends ActionWithoutCibleOps

    class ActionCibled(val action: Action, val cible: List[TimedTrait[_]]) extends ActionCtx
  }

  trait ActionWithoutCibleOps extends ActionCtx {
    override def cible: List[TimedTrait[_]] = Nil
  }


  def fromStdIn(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = {
    println(s"choisir action de ${d.simpleName}")
    Future.successful(fromStdIn match {
      case Attaque.MainGauche => Attaque.MainGauche.fromStdIn(cible)
      case Attaque.MainDroite => Attaque.MainDroite.fromStdIn(cible)
      case Garde => ActionCtx.Garde
      case Rien => ActionCtx.Rien
      case _ => ActionCtx.Rien
    })
  }

  def unapply(str: String): Option[Action] = {
    str match {
      case Attaque.MainGauche.name => Some(Attaque.MainGauche)
      case Attaque.MainDroite.name => Some(Attaque.MainDroite)
      case Action.Rien.name => Some(Rien)
      case _ => None
    }
  }


  def fromStdIn: Action = {
    fromStdin(Action.commonValues)
  }


  def readCibleRec(cible: List[TimedTrait[_]]): List[TimedTrait[_]] = {
    def f(t: TimedTrait[_]) = t.simpleName

    List(fromStdin(cible, f))
  }



}










