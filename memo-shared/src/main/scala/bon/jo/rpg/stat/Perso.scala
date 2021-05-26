package bon.jo.rpg.stat


//import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Actor.{Weapon, WeaponBaseState}
import bon.jo.rpg.{CommandeResolver, AffectResolverTPA}
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.{Affect,Commande, AffectResolver, Timed, TimedTrait}
import bon.jo.rpg.BattleTimeLine._
import scala.collection.mutable
import bon.jo.rpg.AffectResolver.Resolver
import bon.jo.rpg.Affect.Attaque
import bon.jo.rpg.AffectResolver
import bon.jo.rpg.BattleTimeLine
import bon.jo.rpg.resolve.PersoResolveContext._



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
       given PersoOps = new PersoOps 
    }



  object PeroPero extends Timed[GameElement]:


    override def speed(a: GameElement): Int = (a.asInstanceOf[Perso].stats.viv / 10f).round

    override def simpleName(value: GameElement): String = value.asInstanceOf[Perso].name

    override def canChoice(a: GameElement): List[Commande] = 
      val perso = a.asInstanceOf[Perso]
      perso.armesCommandes()

  given  Timed[GameElement] = PeroPero




  class PersoOps(using   PlayerUI
  , ResolveContext) extends  CommandeResolver.Dispatcher[ TimedTrait[Perso], TimedTrait[GameElement]]
  with  AffectResolverTPA[TimedTrait[Perso], TimedTrait[GameElement]] :
    val ctx =  summon[ResolveContext]
    import ctx.given
    def extractResolveArme(a: TimedTrait[Perso], b: Iterable[TimedTrait[GameElement]])(wl : WeaponBaseState):scala.collection.Iterable[UpdateGameElement]=
      (for ac <- wl.affects yield
              ac match
                case given Affect.Soin.type=>   
                  resolveAffect[Affect.Soin.type](a,b)
                case given Affect.Attaque.type=>   
                  resolveAffect[Affect.Attaque.type](a,b)
                case given Affect.Cancel.type=>   
                  resolveAffect[Affect.Cancel.type](a,b)
                case z => 
                  summon[PlayerUI].message("Mais sa fait encore rien",0)
                  Nil
      ).flatten
                
    def resolveCommand(a: TimedTrait[Perso], action: Commande, b: Iterable[TimedTrait[GameElement]]): Iterable[UpdateGameElement] =
     
      action match
        case Commande.Attaque(arme,_)=> extractResolveArme(a,b)(arme)

        case z => 
          summon[PlayerUI].message("Mais sa fait encore rien",0)
          Nil
      







case class Perso(  id : Int, name: String,desc : String, stats : AnyRefBaseStat[Int], hpVar: Int ,lvl : Int , commandes : List[Commande] ,
   leftHandWeapon: Option[Weapon],
rightHandWeapon: Option[Weapon] 
                  ) extends Actor with GameElement with StatsWithName:

  def this(  id : Int, name: String,desc : String, stats : AnyRefBaseStat[Int], lvl : Int = 1, commandes : List[Commande] = Nil,
   leftHandWeapon: Option[Weapon]= None,
rightHandWeapon: Option[Weapon] = None
                ) = this(id,name,desc,stats,stats.hp,lvl,commandes,leftHandWeapon,rightHandWeapon)
  def randomWeapon() =
     copy(leftHandWeapon = Some(randomSoin(Actor.randomWeapon())),rightHandWeapon = Some(randomSoin(Actor.randomWeapon())))
  override def withId[A <: StatsWithName](id: Int): A = copy(id= id).asInstanceOf[A]


