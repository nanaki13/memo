package bon.jo.rpg.stat

import bon.jo.rpg.Action
import bon.jo.rpg.stat.raw.IntBaseStat

trait StatsWithName {
   // self : IntBaseStat =>
    val name : String
    val id : Int
    val action : List[Action]
    val stats : IntBaseStat

}
