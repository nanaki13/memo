package bon.jo.rpg.stat

import bon.jo.rpg.{Action, RandomName}
import bon.jo.rpg.stat.Actor.{ActorBaseStats, WeaponBaseState}
import bon.jo.rpg.stat.BaseState.ImplicitCommon
import bon.jo.rpg.stat.BaseState.ImplicitCommon._
import bon.jo.rpg.stat.raw.IntBaseStat

import scala.reflect.ClassTag
import scala.util.Random

abstract class Actor extends ActorBaseStats with ArmedActor {
  var hpVar: Int  = stats.hp
}

object Actor {


   case class Impl(
                    name : String,
                    lvl:Int,
                    stats : IntBaseStat,
                    leftHandWeapon: Option[WeaponBaseState]= None,
                    rightHandWeapon: Option[WeaponBaseState] = None,
                    action:List[Action] = Nil,id: Int = Id[Impl]
            ) extends Actor with ArmedActor {
  }

  trait Lvl {
    /**
     * indicateur général - permet d’utiliser les armes d’un niveau équivalent.
     *
     * @return
     */
    val lvl: Int
  }

  trait ActorBaseStats extends StatsWithName with Lvl

  val r = new Random()

  object Implicit {
   // implicit val actorFFact: AnyRefBaseStat[Float] => Actor = e => new Actor.Impl(e.to(ImplicitCommon.genFloatToInt))
 //   implicit val wFFact: AnyRefBaseStat[Float] => Weapon = e => new Weapon(e.to(ImplicitCommon.genFloatToInt))
   // implicit val actorFact: AnyRefBaseStat[Int] => Actor = new Actor.Impl(_)
 //   implicit val wFact: AnyRefBaseStat[Int] => Weapon = new Weapon(_)

  }

  def randomActor[A <: Actor](create: AnyRefBaseStat[Int] => A): A = {

   // println((BaseState.`1` * AnyRefBaseStat(randomActorVal _)))
    create((BaseState.`1` * AnyRefBaseStat(randomActorVal _)).to[AnyRefBaseStat[Int]])
  }

  import Implicit._

  def randomActorVal(): Float = randomAround(50, 10)

  def randomWeaponVal(): Float = randomAround(10, 5)

  def randomAround(center: Int, deltaRandom: Int): Float = (center + deltaRandom * (r.nextGaussian() - 0.5)).toFloat

  def randomActor: Actor = {

    val  a= (BaseState.`1` * AnyRefBaseStat(randomActorVal _)).map(_.round)
    println(a)
    var ret: Actor.Impl = raw.Actor.Impl(RandomName(),1,a)

    ret = if (r.nextDouble() < 0.5) {
      val a: Option[WeaponBaseState] = Option(randomWeapon())
      ret.copy(leftHandWeapon = a)
    }else ret
    ret = if (r.nextDouble() < 0.5) {
      val a: Option[WeaponBaseState] = Option(randomWeapon())
      ret.copy(rightHandWeapon = a)
    } else ret

    ret
  }


  def randomWeapon(): Weapon = {
    val stat = (BaseState.`1` * AnyRefBaseStat(randomWeaponVal _))

    Weapon(0,"nom",1,stat, Action.Attaque :: Nil)
  }

  trait WeaponBaseState extends StatsWithName with Lvl

  object Id{
    private val cache = scala.collection.mutable.Map[Class[_],Int]()
    def apply[A](implicit ct : ClassTag[A]):Int = {
      val id =  cache.get(ct.runtimeClass) match {
        case Some(value) => value+1
        case None => 0
      }
      cache(ct.runtimeClass) = id
      id
    }
    def init[A](int: Int)(implicit ct : ClassTag[A]):Unit={
      cache += ct.runtimeClass -> int
    }
  }
   case class Weapon(id : Int ,name : String,lvl : Int,stats: AnyRefBaseStat[Int], action: List[Action]=Action.Attaque :: Nil ) extends  WeaponBaseState {

  }
}
