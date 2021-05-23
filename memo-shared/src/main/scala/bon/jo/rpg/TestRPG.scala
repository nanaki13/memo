package bon.jo.rpg

import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Perso.{PeroPero, WithUI}
import bon.jo.rpg.stat.{AnyRefBaseStat, Perso}
import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.{PlayerUI, PlayerUIStdIn}

import scala.concurrent.ExecutionContext.Implicits.global
import bon.jo.rpg.BattleTimeLine.TimeLineOps
import Perso.given

object TestRPG extends App:
  var id = 0

  def getid() =
    id += 1
    id

    
  given TimeLineParam =  TimeLineParam(0, 50, 70)
   
  given yl :  TimeLineOps = TimeLineOps()
  given PlayerUI = PlayerUIStdIn.value

  val ui : WithUI = new WithUI()


  val p1 = new Perso(1, "Bob", "Un bon gars", AnyRefBaseStat.randomInt(50, 10))
  val p2 = new Perso(2, "Bill", "Un bon gars", AnyRefBaseStat.randomInt(50, 10))


  import ui.given



  yl.add(p1)
  yl.add(p2)


  for _ <- 1 to 100 do


    yl.nextState(yl.timedObjs)












