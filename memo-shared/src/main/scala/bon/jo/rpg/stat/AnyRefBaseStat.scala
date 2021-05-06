package bon.jo.rpg.stat
import bon.jo.rpg.stat.BaseState.ImplicitCommon._

import scala.util.Random
trait AnyRefBaseStat[A] extends Product {
  def growPercent(percent: AnyRefBaseStat[A])
                 (implicit cv: A => Float, s: AnyRefBaseStat[A] => AnyRefBaseStat[Float]): AnyRefBaseStat[Float] = {

    this * (BaseState.`1f` + (percent / (100f)))

  }

  def toPropList: List[A] = {
    productIterator.map(_.asInstanceOf[A]).toList
  }
  def toIterator: Iterator[(String,A)]={
    productElementNames.zipWithIndex.map(e => e._1 -> e.productElement(e._2).asInstanceOf[A])
  }
  def toMap: Map[String,A]={
    toIterator.toMap
  }

  def applyFloat(op: (A, Float) => Float, baseState: Float): AnyRefBaseStat[Float] = {
    AnyRefBaseStat.Impl[Float](
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

  def applyFloat(op: (A, Float) => Float, baseState: AnyRefBaseStat[Float]): AnyRefBaseStat[Float] = {
    AnyRefBaseStat.Impl[Float](
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

  def +[B](baseState: AnyRefBaseStat[B])(implicit cv: A => B,
                                                 cvB: AnyRefBaseStat[A] => AnyRefBaseStat[B]
                                                 , f: Alg[B]
  ): AnyRefBaseStat[B] = {
    AnyRefBaseStat[B, A](this, baseState, (a: B, b: B) => a + b)
  }

  def -[B ](baseState: AnyRefBaseStat[B])(implicit cv: A => B,
                                                 cvB: AnyRefBaseStat[A] => AnyRefBaseStat[B]
                                                 , f: Alg[B]
  ): AnyRefBaseStat[B] = {
    AnyRefBaseStat[B, A](this, baseState, (a: B, b: B) => a - b)
  }

  def *[B ](baseState: AnyRefBaseStat[B])(implicit cv: A => B,
                                                 cvB: AnyRefBaseStat[A] => AnyRefBaseStat[B]
                                                 , f: Alg[B]
  ): AnyRefBaseStat[B] = {
    AnyRefBaseStat[B, A](this, baseState, (a: B, b: B) => a * b)
  }


  def /[B ](baseState: AnyRefBaseStat[B])(implicit cv: A => B,
                                                 cvB: AnyRefBaseStat[A] => AnyRefBaseStat[B]
                                                 , f: Alg[B]
  ): AnyRefBaseStat[B] = {
    AnyRefBaseStat[B, A](this, baseState, (a: B, b: B) => a / b)
  }

  def /[B ](baseState: B)(implicit cv: A => B, cvB: AnyRefBaseStat[A] => AnyRefBaseStat[B]
                                   , f: Alg[B]): AnyRefBaseStat[B] = {
    this / AnyRefBaseStat(baseState)
  }



  def +(baseState: Float)(implicit cv: A => Float, cvB: AnyRefBaseStat[A] => AnyRefBaseStat[Float]
  ): AnyRefBaseStat[Float] = {
    this + AnyRefBaseStat(baseState)
  }

  def -(baseState: Float)(implicit cv: A => Float, cvB: AnyRefBaseStat[A] => AnyRefBaseStat[Float]
  ): AnyRefBaseStat[Float] = {
    this - AnyRefBaseStat(baseState)
  }


  def to[T <: AnyRefBaseStat[_]](implicit cv: AnyRefBaseStat[A] => T): T = cv(this)

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
object AnyRefBaseStat{

  val r = new Random()

  def randomInt( center : Int,delta : Int): AnyRefBaseStat[Int] = apply[Int](()=>center + (delta*(1d-r.nextGaussian())).round.toInt)
  case class Impl[A](
                      override val hp: A,
                      override val sp: A,
                      override val viv: A,
                      override val str: A,
                      override val mag: A,
                      override val vit: A,
                      override val psy: A,
                      override val res: A,
                      override val chc: A
                    ) extends AnyRefBaseStat[A]{

  }

    def apply[A , T ](baseState: AnyRefBaseStat[T])(implicit cv: T => A) = {
       Impl[A](
        cv(baseState.hp),
        cv(baseState.sp),
        cv(baseState.viv),
        cv(baseState.str),
        cv(baseState.mag),
        cv(baseState.vit),
        cv(baseState.psy),
        cv(baseState.res),
        cv(baseState.chc)
      )
    }

    def apply[A](iterable: Iterable[(String,A)]): AnyRefBaseStat[Option[A]] ={
      val map = iterable.toMap
      Impl(
        map.get("hp"),
        map.get("sp"),
        map.get("viv"),
        map.get("str"),
        map.get("mag"),
        map.get("vit"),
        map.get("psy"),
        map.get("res"),
        map.get("chc")
      )
    }
    def apply[A](c: ()=>A) = {
      println("APPLY")
      val e = Impl(c(), c(), c(), c(), c(), c(), c(), c(), c())
      println(e)
      e
    }
    def apply[A](c: A) = {
      Impl(c, c, c, c, c, c, c, c, c)
    }

    def apply[A , T ](baseState: AnyRefBaseStat[T], baseStateT: AnyRefBaseStat[A], opF: (A, A) => A)(implicit cv: T => A) = {
      Impl(
        opF(cv(baseState.hp), baseStateT.hp),
        opF(cv(baseState.sp), baseStateT.sp),
        opF(cv(baseState.viv), baseStateT.viv),
        opF(cv(baseState.str), baseStateT.str),
        opF(cv(baseState.mag), baseStateT.mag),
        opF(cv(baseState.vit), baseStateT.vit),
        opF(cv(baseState.psy), baseStateT.psy),
        opF(cv(baseState.res), baseStateT.res),
        opF(cv(baseState.chc), baseStateT.chc),
      )
    }


}
