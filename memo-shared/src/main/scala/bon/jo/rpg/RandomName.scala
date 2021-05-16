package bon.jo.rpg

import scala.util.Random

object RandomName {
  val sylab = List("bou", "ba", "bi", "cla", "d", "ri", "dim", "me", "ro", "zi", "na", "ki", "r")

  def apply(): String = {
    val r = new Random()
    val nbSyl = r.nextInt(5)
    val ret = (for (i <- 0 to nbSyl) yield sylab(r.nextInt(sylab.size))).mkString("")
    ret.head.toUpper +: ret.tail
  }
}
