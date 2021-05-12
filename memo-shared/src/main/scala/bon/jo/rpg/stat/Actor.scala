package bon.jo.rpg.stat

import bon.jo.rpg.Action
import bon.jo.rpg.stat.Actor.{ActorBaseStats, WeaponBaseState}
import bon.jo.rpg.stat.BaseState.ImplicitCommon
import bon.jo.rpg.stat.BaseState.ImplicitCommon._

import scala.reflect.ClassTag
import scala.util.Random

abstract case class Actor(
             override var lvl: Int,
             var hpVar: Int,
             override val sp: Int,
             override val viv: Int,
             override val str: Int,
             override val mag: Int,
             override val vit: Int,
             override val psy: Int,
             override val res: Int,
             override val chc: Int,
             override var leftHand: Option[WeaponBaseState] = None,
             override var rightHand: Option[WeaponBaseState] = None
           ) extends AnyRefBaseStat[Int] with ActorBaseStats with ArmedActor {


  def this(baseState: AnyRefBaseStat[Int]) = this(1, baseState.hp,
    baseState.sp,
    baseState.viv, baseState.str, baseState.mag, baseState.vit, baseState.psy, baseState.res, baseState.chc)

  /**
   * indique la santé du personnage
   *
   * @return
   */
  override def hp: Int = hpVar
}

object Actor {


   class Impl(
              lvl: Int,
               hpVar: Int,
               sp: Int,
              viv: Int,
             str: Int,
               mag: Int,
              vit: Int,
               psy: Int,
             res: Int,
            chc: Int,
               leftHand: Option[WeaponBaseState] = None,
            rightHand: Option[WeaponBaseState] = None
            ) extends Actor(lvl,hpVar,sp,viv, str, mag, vit, psy, res, chc, leftHand, rightHand){
    def this(baseState: AnyRefBaseStat[Int]) = this(1, baseState.hp,
      baseState.sp,
      baseState.viv, baseState.str, baseState.mag, baseState.vit, baseState.psy, baseState.res, baseState.chc)
  }

  trait Lvl {
    /**
     * indicateur général - permet d’utiliser les armes d’un niveau équivalent.
     *
     * @return
     */
    var lvl: Int
  }

  trait ActorBaseStats extends AnyRefBaseStat[Int] with Lvl

  val r = new Random()

  object Implicit {
    implicit val actorFFact: AnyRefBaseStat[Float] => Actor = e => new Actor.Impl(e.to(ImplicitCommon.genFloatToInt))
 //   implicit val wFFact: AnyRefBaseStat[Float] => Weapon = e => new Weapon(e.to(ImplicitCommon.genFloatToInt))
    implicit val actorFact: AnyRefBaseStat[Int] => Actor = new Actor.Impl(_)
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

    val  a= (BaseState.`1` * AnyRefBaseStat(randomActorVal _))
    println(a)
    val ret: Actor = (BaseState.`1` * AnyRefBaseStat(randomActorVal _)).to[Actor]

    if (r.nextDouble() < 0.5) {
      val a: Option[WeaponBaseState] = Option(randomWeapon())
      ret.leftHand = a
    }
    if (r.nextDouble() < 0.5) {
      val a: Option[WeaponBaseState] = Option(randomWeapon())
      ret.rightHand = a
    }

    ret
  }


  def randomWeapon(): Weapon = {
    val stat = (BaseState.`1` * AnyRefBaseStat(randomWeaponVal _))

    new Weapon("nom",stat, Action.Attaque :: Nil)
  }

  trait WeaponBaseState extends StatsWithName with Lvl {
    var action: List[Action] = List(Action.Attaque)
  }

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
  }
   class Weapon(    val name : String,
                     override var lvl: Int,
                     override val hp: Int,
                     override val sp: Int,
                     override val viv: Int,
                     override val str: Int,
                     override val mag: Int,
                     override val vit: Int,
                     override val psy: Int,
                     override val res: Int,
                     override val chc: Int,val id : Int = Id[Weapon]
                   ) extends AnyRefBaseStat.Impl[Int](hp, sp, viv, str, mag, vit, psy, res, chc) with WeaponBaseState {
    def this(name : String,baseState: AnyRefBaseStat[Int]) = this(name,1, baseState.hp,
      baseState.sp,
      baseState.viv, baseState.str, baseState.mag, baseState.vit, baseState.psy, baseState.res, baseState.chc)

     def this(name : String,baseState: AnyRefBaseStat[Int],action : List[Action]) = {
       this(name,baseState)
       this.action = action
     }
  }
}
