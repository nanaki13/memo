package bon.jo.rpg.stat

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Actor.WeaponBaseState
import bon.jo.rpg.stat.Perso.getid
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.{Action, ActionResolver, Timed, TimedTrait}

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
  class WithUI()(implicit  playerUI: PlayerUI){
    implicit object o extends PersoOps {
      override val ui: PlayerUI = playerUI
    }
    implicit val value :  ActionResolver[Perso, List[TimedTrait[_]]] = o
    implicit val acImpl: ActionResolver[TimedTrait[Any], List[TimedTrait[_]]] = {
      (a: TimedTrait[_], action: Action, b: List[TimedTrait[_]]) =>
        a.value match {
          case e: Perso => e.resolve(action, b.value)
        }
    }
  }


  implicit object PeroPero extends Timed[Perso] {

    val posCache = mutable.Map[Int, Int]()


    override def speed(a: Perso): Int = (a.stats.viv / 10f).round

    override def action_=(a: Perso, action: ActionCtx): Unit = a.actionCtx = Some(action)

    override def action(a: Perso): ActionCtx = a.actionCtx.getOrElse(ActionCtx.Rien)

    override def pos(a: Perso): Int = {
      posCache.getOrElse(a.id, 0)
    }

    override def pos_=(a: Perso, pos: Int): Unit = posCache(a.id) = pos

    override def simpleName(value: Perso): String = value.name

    override def canChoice(a: Perso): List[Action] = a.action
  }


  trait PersoOps extends ActionResolver[Perso, List[TimedTrait[_]]] {
    val ui : PlayerUI
    def resolve(a: Perso, action: Action, b: List[TimedTrait[_]]): Unit = {
      action match {
        case Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Attaque=>
          b.map(_.value) match {
            case List(p: Perso) =>
              p.hpVar -= a.stats.str
              ui.message(s"${p.name} a perdu ${a.stats.str} pv, il lui reste ${p.hpVar} pv",5000)
              ui.cpntMap(p.asInstanceOf[ui.S]).update(Some(p.asInstanceOf[ui.S]))
            case _ =>
          }
        case Action.Garde =>
        case Action.Rien =>
        case Action.Soin=>
          b.map(_.value) match {
            case List(p: Perso) =>
              p.hpVar += (a.stats.mag * 0.7f).round
              ui.message(s"${p.name} a été soigné de ${(a.stats.mag * 0.7f).round} pv, il a maintenant ${p.hpVar} pv",5000)
              ui.cpntMap(p.asInstanceOf[ui.S]).update(Some(p.asInstanceOf[ui.S]))
            case _ =>
          }
      }


    }
  }

  def apply( name: String, stat : AnyRefBaseStat[Int]): Perso ={
    new Perso(name,stat)
  }

}
 case class Perso( name: String, stats : AnyRefBaseStat[Int],lvl : Int = 1, action : List[Action] = Nil,
   leftHandWeapon: Option[WeaponBaseState]= None,
rightHandWeapon: Option[WeaponBaseState] = None
                   , id: Int = getid()) extends Actor with StatsWithName{


   def randomWeapon() = {
     copy(leftHandWeapon = Some(randomSoin(Actor.randomWeapon())),rightHandWeapon = Some(randomSoin(Actor.randomWeapon())))

   }
 }


