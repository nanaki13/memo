package bon.jo.rpg.stat

import bon.jo.rpg.stat.BaseState.BaseStatImpl
import bon.jo.rpg.stat.BaseState.ImplicitCommon._

import scala.util.Random
object GenBaseState {
  val r = new Random()
  def apply[A <: AnyVal](a: A): GenBaseState[A] = {
    BaseStatImpl(a)
  }

  def apply[A <: AnyVal](a: () => A): GenBaseState[A] = {
    BaseStatImpl(a)
  }
  def randomInt( center : Int,delta : Int): GenBaseState[Int] = apply[Int](()=>center + (delta*(1d-r.nextGaussian())).round.toInt)
}

trait GenBaseState[A <: AnyVal] {

  def growPercent(percent: GenBaseState[A])
                 (implicit cv: A => Float, s: GenBaseState[A] => GenBaseState[Float]): GenBaseState[Float] = {

    this * (BaseState.`1f` + (percent / (100f)))

  }

  def applyFloat(op: (A, Float) => Float, baseState: Float): GenBaseState[Float] = {
    new BaseStatImpl[Float](
      op(hp, baseState),
      op(sp, baseState),
      op(viv, baseState),
      op(str, baseState),
      op(mag, baseState),
      op(vit, baseState),
      op(psy, baseState),
      op(res, baseState),
      op(chc, baseState))
  }

  def applyFloat(op: (A, Float) => Float, baseState: GenBaseState[Float]): GenBaseState[Float] = {
    new BaseStatImpl[Float](
      op(hp, baseState.hp),
      op(sp, baseState.sp),
      op(viv, baseState.viv),
      op(str, baseState.str),
      op(mag, baseState.mag),
      op(vit, baseState.viv),
      op(psy, baseState.psy),
      op(res, baseState.res),
      op(chc, baseState.chc))
  }


  implicit class WithAlg[B: Alg](val a: B) {
    val alg = implicitly[Alg[B]]

    def +(b: B): B = alg + (a, b)

    def -(b: B): B = alg - (a, b)

    def *(b: B): B = alg * (a, b)

    def /(b: B): B = alg / (a, b)
  }

  def +[B <: AnyVal](baseState: GenBaseState[B])(implicit cv: A => B,
                                                 cvB: GenBaseState[A] => GenBaseState[B]
                                                 , f: Alg[B]
  ): GenBaseState[B] = {
    BaseStatImpl[B, A](this, baseState, (a: B, b: B) => a + b)
  }

  def -[B <: AnyVal](baseState: GenBaseState[B])(implicit cv: A => B,
                                                 cvB: GenBaseState[A] => GenBaseState[B]
                                                 , f: Alg[B]
  ): GenBaseState[B] = {
    BaseStatImpl[B, A](this, baseState, (a: B, b: B) => a - b)
  }

  def *[B <: AnyVal](baseState: GenBaseState[B])(implicit cv: A => B,
                                                 cvB: GenBaseState[A] => GenBaseState[B]
                                                 , f: Alg[B]
  ): GenBaseState[B] = {
    BaseStatImpl[B, A](this, baseState, (a: B, b: B) => a * b)
  }


  def /[B <: AnyVal](baseState: GenBaseState[B])(implicit cv: A => B,
                                                 cvB: GenBaseState[A] => GenBaseState[B]
                                                 , f: Alg[B]
  ): GenBaseState[B] = {
    BaseStatImpl[B, A](this, baseState, (a: B, b: B) => a / b)
  }

  def /[B <: AnyVal](baseState: B)(implicit cv: A => B, cvB: GenBaseState[A] => GenBaseState[B]
                                   , f: Alg[B]): GenBaseState[B] = {
    this / GenBaseState(baseState)
  }



  def +(baseState: Float)(implicit cv: A => Float, cvB: GenBaseState[A] => GenBaseState[Float]
  ): GenBaseState[Float] = {
    this + GenBaseState(baseState)
  }

  def -(baseState: Float)(implicit cv: A => Float, cvB: GenBaseState[A] => GenBaseState[Float]
  ): GenBaseState[Float] = {
    this - GenBaseState(baseState)
  }


  def to[T <: GenBaseState[_]](implicit cv: GenBaseState[A] => T): T = cv(this)


  /**
   * indique la santé du personnage
   *
   * @return
   */
  def hp: A

  /**
   * permet d’utiliser les talents
   *
   * @return
   */
  def sp: A

  /**
   * Gouverne la vitesse à laquelle le personnage se déplace sur la Timeline de combat.
   *
   * @return
   */
  def viv: A

  /**
   * Ces points s'ajoutent au calcul des attaques physiques
   *
   * @return
   */
  def str: A

  /**
   * Ces points s’ajoutent au calcul des attaques magiques
   *
   * @return
   */
  def mag: A

  /**
   * Ces points servent au calcul de la défense physique
   *
   * @return
   */
  def vit: A

  /**
   * Ces points servent au calcul de la défense magique
   *
   * @return
   */
  def psy: A

  /**
   * Augmente les chances d’échapper à un effet néfaste
   *
   * @return
   */
  def res: A

  /**
   * Gouverne les chance de réaliser un crit (x2 dégâts!)
   *
   * @return
   */
  def chc: A

  override def toString = s"GenBaseState($hp, $sp, $viv, $str, $mag, $vit, $psy, $res, $chc)"
}