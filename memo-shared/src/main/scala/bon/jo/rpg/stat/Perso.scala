package bon.jo.rpg.stat

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Actor.{Weapon, WeaponBaseState}
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.{Action, ActionResolver, Timed, TimedTrait}
import bon.jo.rpg.BattleTimeLine._
import scala.collection.mutable

object Perso:

  type Weapons = Option[(Option[Weapon],Option[Weapon])]
  object ArmePerso:
    def unapply(e : StatsWithName):Weapons=
      e match
        case Perso(_,_,_,_, _, _, _, l, r) => Some(l,r)
        case _ => None

  trait PlayerPersoUI extends PlayerUI:
    type S = Perso
  class WithUI()(using PlayerUI):

    
    given ActionResolver[Perso, TimedTrait[GameElement]] = new PersoOps{}
    given ActionResolver[TimedTrait[GameElement], TimedTrait[GameElement]] =
          new  ActionResolver[TimedTrait[GameElement], TimedTrait[GameElement]]{
            override def resolve(a: TimedTrait[GameElement], action: Action, b: List[TimedTrait[GameElement]]): List[UpdateGameElement] = a.value match
              case e: Perso => e.resolve(action, b.value)
              case _ => Nil
          }



  object PeroPero extends Timed[GameElement]:


    override def speed(a: GameElement): Int = (a.asInstanceOf[Perso].stats.viv / 10f).round

    override def simpleName(value: GameElement): String = value.asInstanceOf[Perso].name

    override def canChoice(a: GameElement): List[Action] = a.asInstanceOf[Perso].action

  given  Timed[GameElement] = PeroPero
  trait PersoOps(using ui :  PlayerUI) extends ActionResolver[Perso, TimedTrait[GameElement]]:
    def resolve(a: Perso, action: Action, b: List[TimedTrait[GameElement]]): List[UpdateGameElement] =
      action match
        case Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Attaque=>
          b.map{ e =>
            e.value match
              case _ : Perso =>
                UpdateGameElement(e.id,(perTe: TPA) =>{
                  val per = perTe.value.asInstanceOf[Perso]
                  val newPerso = per.copy(hpVar = per.hpVar - a.stats.str)
                  ui.message(s"${per.name} a perdu ${per.stats.str} pv, il lui reste ${newPerso.hpVar} pv",5000)
                  ui.cpntMap(e.id).update(Some(newPerso))
                  perTe.withValue(newPerso)})
              case a @ _ => println(s" not handle : $a");???
          }

        case Action.Soin=>
          b.map{ e =>
            e.value match
              case _ : Perso =>
                UpdateGameElement(e.id,(perTe: TPA) =>{
                  val per = perTe.value.asInstanceOf[Perso]
                  val newPerso =per.copy(hpVar = per.hpVar + (a.stats.mag * 0.7f).round)
                  ui.message(s"${per.name} a été soigné de ${(a.stats.mag * 0.7f).round} pv, il a maintenant ${newPerso.hpVar} pv",5000)
                  ui.cpntMap(e.id).update(Some(newPerso))
                  perTe.withValue(newPerso)})
              case _ => ???
          }
        case z => 
          ui.message("Mais sa fait encore rien",0)
          Nil








case class Perso(  id : Int, name: String,desc : String, stats : AnyRefBaseStat[Int], hpVar: Int ,lvl : Int , action : List[Action] ,
   leftHandWeapon: Option[Weapon],
rightHandWeapon: Option[Weapon] 
                  ) extends Actor with GameElement with StatsWithName:

  def this(  id : Int, name: String,desc : String, stats : AnyRefBaseStat[Int], lvl : Int = 1, action : List[Action] = Nil,
   leftHandWeapon: Option[Weapon]= None,
rightHandWeapon: Option[Weapon] = None
                ) = this(id,name,desc,stats,stats.hp,lvl,action,leftHandWeapon,rightHandWeapon)
  def randomWeapon() =
     copy(leftHandWeapon = Some(randomSoin(Actor.randomWeapon())),rightHandWeapon = Some(randomSoin(Actor.randomWeapon())))
  override def withId[A <: StatsWithName](id: Int): A = copy(id= id).asInstanceOf[A]


