package bon.jo.rpg.stat

import bon.jo.rpg.Action
import bon.jo.rpg.stat.raw.IntBaseStat

trait StatsWithName {
  // self : IntBaseStat =>

  val name: String
  val id: Int
  val desc : String
  val action: List[Action]
  val stats: IntBaseStat

  def withId[A <: StatsWithName](id: Int): A

}
