package bon.jo.rpg.stat

import bon.jo.rpg.Action
import bon.jo.rpg.stat.raw.IntBaseStat

trait StatsWithName extends IntBaseStat{
   // self : IntBaseStat =>
    def name : String
    var action : List[Action]
}
