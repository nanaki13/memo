package bon.jo.rpg.stat





object BaseState {

  object `0` extends BaseStatImpl[Int](0, 0, 0, 0, 0, 0, 0, 0, 0)

  object `1` extends BaseStatImpl[Int](1, 1, 1, 1, 1, 1, 1, 1, 1)

  object `0f` extends BaseStatImpl[Float](0, 0, 0, 0, 0, 0, 0, 0, 0)

  object `1f` extends BaseStatImpl[Float](1, 1, 1, 1, 1, 1, 1, 1, 1)

  object `0d` extends BaseStatImpl[Double](0, 0, 0, 0, 0, 0, 0, 0, 0)

  object `1d` extends BaseStatImpl[Double](1, 1, 1, 1, 1, 1, 1, 1, 1)

  object ImplicitCommon {
    implicit val iToF: Int => Float = e => e.toFloat
    implicit val fToI: Float => Int = e => e.round
    implicit val genIntToFloat: GenBaseState[Int] => GenBaseState[Float] = BaseStatImpl[Float, Int](_)
    implicit val genFloatToInt: GenBaseState[Float] => GenBaseState[Int] = BaseStatImpl[Int, Float](_)
    implicit val algFloat  : Alg[Float] = new Alg[Float] {
      override def +(a: Float, b: Float): Float = a+b

      override def -(a: Float, b: Float): Float = a-b

      override def *(a: Float, b: Float): Float = a*b

      override def /(a: Float, b: Float): Float = a/b
    }
    implicit val algInt  : Alg[Int] = new Alg[Int] {
      override def +(a: Int, b: Int): Int = a+b

      override def -(a: Int, b: Int): Int = a-b

      override def *(a: Int, b: Int): Int = a*b

      override def /(a: Int, b: Int): Int = a/b
    }
  }

  class BaseStatImpl[T <: AnyVal](
                                   override val hp: T,
                                   override val sp: T,
                                   override val viv: T,
                                   override val str: T,
                                   override val mag: T,
                                   override val vit: T,
                                   override val psy: T,
                                   override val res: T,
                                   override val chc: T
                                 ) extends GenBaseState[T] {
    def this(baseState: GenBaseState[T]) = this(baseState.hp,
      baseState.sp,
      baseState.viv, baseState.str, baseState.mag, baseState.vit, baseState.psy, baseState.res, baseState.chc)
  }

  object BaseStatImpl {
    def apply[A <: AnyVal, T <: AnyVal](baseState: GenBaseState[T])(implicit cv: T => A) = {
      new BaseStatImpl[A](
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
    def apply[A <: AnyVal](c: ()=>A) = {
      new BaseStatImpl[A](c(), c(), c(), c(), c(), c(), c(), c(), c())
    }
    def apply[A <: AnyVal](c: A) = {
      new BaseStatImpl[A](c, c, c, c, c, c, c, c, c)
    }

    def apply[A <: AnyVal, T <: AnyVal](baseState: GenBaseState[T], baseStateT: GenBaseState[A], opF: (A, A) => A)(implicit cv: T => A) = {
      new BaseStatImpl[A](
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




}


