package bon.jo.rpg.stat

trait Alg[A] {
  def +(a: A, b: A): A

  def -(a: A, b: A): A

  def *(a: A, b: A): A

  def /(a: A, b: A): A
}
object Alg
