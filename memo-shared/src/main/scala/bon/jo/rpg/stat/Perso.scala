package bon.jo.rpg.stat

import bon.jo.rpg.Action.ActionCtx
//import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Actor.{Weapon, WeaponBaseState}
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.{Action, ActionResolver, Timed, TimedTrait}
import bon.jo.rpg.BattleTimeLine._
import scala.collection.mutable
import bon.jo.rpg.ActionResolver.Resolver
import bon.jo.rpg.Action.Attaque
import bon.jo.rpg.ActionResolver.ActionResolverWithResolver
import bon.jo.rpg.BattleTimeLine


type AttaqueResolve =  Resolver[TimedTrait[Perso], TimedTrait[GameElement],Action.Attaque.type]
type SoinResolve =  Resolver[TimedTrait[Perso], TimedTrait[GameElement],Action.Soin.type]
type GardeResolve =  Resolver[TimedTrait[Perso], TimedTrait[GameElement],Action.Garde.type]
object ResolveContext:
  def unknwon[A <: Action]():  Resolver[TimedTrait[Perso], TimedTrait[GameElement],A] = 
    new Resolver:
      override def resolve(a: TimedTrait[Perso], b: TimedTrait[GameElement])(using ui : PlayerUI) : TimedTrait[GameElement] =
        ui.message("mais sa fait rien",1000)
        b
trait ResolveContext:
  given attaqueResolve: AttaqueResolve
  given soinResolve: SoinResolve
  given gardeResolve: GardeResolve
  
  given Resolver[TimedTrait[Perso], TimedTrait[GameElement],Action.Rien.type] = ResolveContext.unknwon[Action.Rien.type]()




object Perso:

  type Weapons = Option[(Option[Weapon],Option[Weapon])]
  object ArmePerso:
    def unapply(e : StatsWithName):Weapons=
      e match
        case Perso(_,_,_,_, _, _, _, l, r) => Some(l,r)
        case _ => None

  trait PlayerPersoUI extends PlayerUI:
    type S = Perso
  class WithUI()(using PlayerUI,ResolveContext):
    given WithUI = this
    object PersoResolver{
       given BattleTimeLine.Res = new PersoOps {}
    }



  object PeroPero extends Timed[GameElement]:


    override def speed(a: GameElement): Int = (a.asInstanceOf[Perso].stats.viv / 10f).round

    override def simpleName(value: GameElement): String = value.asInstanceOf[Perso].name

    override def canChoice(a: GameElement): List[Action] = a.asInstanceOf[Perso].action

  given  Timed[GameElement] = PeroPero


  trait PersoOps(using   PlayerUI
  , ResolveContext) extends ActionResolver[ TimedTrait[Perso], TimedTrait[GameElement]] with ActionResolverWithResolver[TimedTrait[Perso], TimedTrait[GameElement]]:
    def resolve(a: TimedTrait[Perso], action: Action, b: Iterable[TimedTrait[GameElement]]): Iterable[UpdateGameElement] =
      val ctx =  summon[ResolveContext]
      import ctx.given
      action match
        case Action.Attaque.MainGauche | Action.Attaque.MainDroite | Action.Attaque=>
          
          val armeActions = (for wl <- a.value.leftHandWeapon yield
            (for ac <- wl.action yield
              ac match
                case  Action.Soin=> resolve[ Action.Soin.type](a,b)
                case  Action.Attaque=> resolve[ Action.Attaque.type](a,b)
                ).flatten).getOrElse(Nil)
               
            
          
          armeActions
          
        case Action.Soin=> resolve[ Action.Soin.type](a,b)

        case z => 
          summon[PlayerUI].message("Mais sa fait encore rien",0)
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


