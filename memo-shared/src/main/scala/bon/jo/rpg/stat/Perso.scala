package bon.jo.rpg.stat

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Actor.{Weapon, WeaponBaseState}
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.{Action, ActionResolver, Timed, TimedTrait}

import scala.collection.mutable

object Perso:

  type Weapons = Option[(Option[Weapon],Option[Weapon])]
  object ArmePerso:
    def unapply(e : StatsWithName):Weapons=
      e match
        case Perso(_,_, _, _, _, _, l, r) => Some(l,r)
        case _ => None

  trait PlayerPersoUI extends PlayerUI:
    type S = Perso
  class WithUI()(implicit  playerUI: PlayerUI):
    implicit object o extends PersoOps:
      override val ui: PlayerUI = playerUI
    implicit val value :  ActionResolver[Perso, List[TimedTrait[_]]] = o
    implicit val acImpl: ActionResolver[TimedTrait[Any], List[TimedTrait[_]]] =
          new  ActionResolver[TimedTrait[Any], List[TimedTrait[_]]]{
            override def resolve(a: TimedTrait[Any], action: Action, b: List[TimedTrait[_]]): Unit = a.value match
              case e: Perso => e.resolve(action, b.value)(value)
          }



  implicit object PeroPero extends Timed[Perso]:

    val posCache = mutable.Map[Int, Int]()


    override def speed(a: Perso): Int = (a.stats.viv / 10f).round

//    override def action_=(a: Perso, action: ActionCtx): Unit = a.actionCtx = Some(action)
//
//    override def action(a: Perso): ActionCtx = a.actionCtx.getOrElse(ActionCtx.Rien)
//
//    override def pos(a: Perso): Int = {
//      posCache.getOrElse(a.id, 0)
//    }
//
//    override def pos_=(a: Perso, pos: Int): Unit = posCache(a.id) = pos

    override def simpleName(value: Perso): String = value.name

    override def canChoice(a: Perso): List[Action] = a.action


  trait PersoOps extends ActionResolver[Perso, List[TimedTrait[_]]]:
    val ui : PlayerUI
    def resolve(a: Perso, action: Action, b: List[TimedTrait[_]]): Unit =
      action match
        case Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Attaque=>
          b.map(_.value) match
            case List(p: Perso) =>
              p.hpVar -= a.stats.str
              ui.message(s"${p.name} a perdu ${a.stats.str} pv, il lui reste ${p.hpVar} pv",5000)
              ui.cpntMap(p.asInstanceOf[ui.S]).update(Some(p.asInstanceOf[ui.S]))
            case _ =>

        case Action.Soin=>
          b.map(_.value) match
            case List(p: Perso) =>
              p.hpVar += (a.stats.mag * 0.7f).round
              ui.message(s"${p.name} a �t� soign� de ${(a.stats.mag * 0.7f).round} pv, il a maintenant ${p.hpVar} pv",5000)
              ui.cpntMap(p.asInstanceOf[ui.S]).update(Some(p.asInstanceOf[ui.S]))
            case _ =>
        case z => ui.message("Mais sa fait encore rien",0)








case class Perso(  id : Int, name: String,desc : String, stats : AnyRefBaseStat[Int],lvl : Int = 1, action : List[Action] = Nil,
   leftHandWeapon: Option[Weapon]= None,
rightHandWeapon: Option[Weapon] = None
                  ) extends Actor with StatsWithName:
  def randomWeapon() =
     copy(leftHandWeapon = Some(randomSoin(Actor.randomWeapon())),rightHandWeapon = Some(randomSoin(Actor.randomWeapon())))
  override def withId[A <: StatsWithName](id: Int): A = copy(id= id).asInstanceOf[A]


