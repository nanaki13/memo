package bon.jo.app

import bon.jo.rpg.BattleTimeLine.TimeLineParam

object GameParams {
    given TimeLineParam = TimeLineParam(0, 200, 260)
}
