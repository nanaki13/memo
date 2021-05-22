package bon.jo.util

trait Mapper[A,B]:
    val map : A => B
    val unmap : B => Option[A]
