package bon.jo.rpg.stat

import bon.jo.rpg.stat.Actor.{ActorBaseStats, WeaponBaseState}
import bon.jo.rpg.stat.BaseState.{BaseStatImpl, ImplicitCommon}
import ImplicitCommon._
import bon.jo.rpg.Action

import scala.util.Random

class Actor(
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
                ) extends GenBaseState[Int] with ActorBaseStats with ArmedActor {





  def this(baseState: GenBaseState[Int]) = this(1, baseState.hp,
    baseState.sp,
    baseState.viv, baseState.str, baseState.mag, baseState.vit, baseState.psy, baseState.res, baseState.chc)

  /**
   * indique la santé du personnage
   *
   * @return
   */
  override def hp: Int = hpVar
}

object Actor{


  trait Lvl {
    /**
     * indicateur général - permet d’utiliser les armes d’un niveau équivalent.
     *
     * @return
     */
    var lvl: Int
  }

  trait ActorBaseStats extends GenBaseState[Int] with Lvl

  val r = new Random()

  object Implicit {
    implicit val actorFFact: GenBaseState[Float] => Actor = e =>  new Actor(e.to(ImplicitCommon.genFloatToInt))
    implicit val wFFact: GenBaseState[Float] => Weapon = e =>  new Weapon(e.to(ImplicitCommon.genFloatToInt))
    implicit val actorFact: GenBaseState[Int] => Actor = new Actor(_)
    implicit val wFact: GenBaseState[Int] => Weapon = new Weapon(_)

  }

  import Implicit._

  def randomActorVal(): Float = randomAround(50, 10)

  def randomWeaponVal(): Float = randomAround(10, 5)

  def randomAround(center: Int, deltaRandom: Int): Float = (center + deltaRandom * (r.nextGaussian() - 0.5)).toFloat

  def randomActor: Actor = {

    val ret: Actor = (BaseState.`1` * GenBaseState(randomActorVal _)).to[Actor]

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
    (BaseState.`1` * GenBaseState(randomWeaponVal _)).to[Weapon]
  }

  trait WeaponBaseState extends GenBaseState[Int] with Lvl{
    var action:List[Action] = List(Action.Attaque)
  }

  case class Weapon(
                     override var lvl: Int,
                     override val hp: Int,
                     override val sp: Int,
                     override val viv: Int,
                     override val str: Int,
                     override val mag: Int,
                     override val vit: Int,
                     override val psy: Int,
                     override val res: Int,
                     override val chc: Int
                   ) extends BaseStatImpl[Int](hp, sp, viv, str, mag, vit, psy, res, chc) with WeaponBaseState {
    def this(baseState: GenBaseState[Int]) = this(1, baseState.hp,
      baseState.sp,
      baseState.viv, baseState.str, baseState.mag, baseState.vit, baseState.psy, baseState.res, baseState.chc)
  }
}
