package bon.jo.phy

import bon.jo.phy.Phy.{A, P, V}

trait PointDynamic {
  var m: Double


  var p: P
  var v: V
  var a: A

  def addDt(t: Double): Unit = {
    val newV = (a sum t) + v
    val newP = (newV sum t) + p
    v = newV
    p = newP
  }

  def x: Double = p.x

  def y: Double = p.y

  def t: Double = p.t
}

object PointDynamic {
  def apply(p_ : P, v: V = V(), a: A = A(), m: Double = 1
            , forces: List[A] = Nil
           ): PointDynamic with WithId = new PointDynamicImpl(p_, v, a, m, forces)
}
