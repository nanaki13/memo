package bon.jo.rpg.stat

import bon.jo.rpg.Action.{ActionCtx, PlayerUI}
import bon.jo.rpg.stat.Perso.getid
import bon.jo.rpg.{Action, Actions, Timed, TimedTrait}

import scala.collection.mutable

object Perso {
  var id = 0

  def getid() = {
    id += 1
    id
  }

  trait PlayerPersoUI extends PlayerUI{
    type S = Perso
  }
  class WithUI()(implicit val playerUI: PlayerUI){
    implicit object o extends PersoOps {
      override val ui: PlayerUI = playerUI
    }
    implicit val value :  Actions[Perso, List[TimedTrait[_]]] = o
  }


  implicit object PeroPero extends Timed[Perso] {

    val posCache = mutable.Map[Int, Int]()


    override def speed(a: Perso): Int = a.vit

    override def action_=(a: Perso, action: ActionCtx): Unit = a.actionCtx = Some(action)

    override def action(a: Perso): ActionCtx = a.actionCtx.getOrElse(ActionCtx.Rien)

    override def pos(a: Perso): Int = {
      posCache.getOrElse(a.id, 0)
    }

    override def pos_=(a: Perso, pos: Int): Unit = posCache(a.id) = pos

    override def simpleName(value: Perso): String = value.name

    override def canChoice(a: Perso): List[Action] = a.action
  }


  trait PersoOps extends Actions[Perso, List[TimedTrait[_]]] {
    val ui : PlayerUI
    def resolve(a: Perso, action: Action, b: List[TimedTrait[_]]): Unit = {
      action match {
        case Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Attaque=>
          b.map(_.value) match {
            case List(p: Perso) =>
              p.hpVar -= a.str
              ui.message(s"${p.name} a perdu ${a.str} pv, il lui reste ${p.hp} pv",5000)
              ui.cpntMap(p.asInstanceOf[ui.S]).update(Some(p.asInstanceOf[ui.S]))
            case _ =>
          }
        case Action.Defendre =>
        case Action.Rien =>
      }


    }
  }

}
case class Perso(name: String, stat : GenBaseState[Int], id: Int = getid()) extends Actor(stat)


