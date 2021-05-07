package bon.jo.common

object Affects {
  trait AffectOps[A, B] {
    def affect(target: A, a: B):Unit
  }

  implicit class Affect[A](v: A) {
    def :=[B](o: B)(implicit a: AffectOps[A, B]):Unit = {
      a.affect(v, o)
    }
  }
}