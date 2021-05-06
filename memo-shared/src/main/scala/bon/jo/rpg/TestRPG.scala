package bon.jo.rpg

import bon.jo.rpg.Action.PlayerUIStdIn
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Perso.{PeroPero, WithUI}
import bon.jo.rpg.stat.{AnyRefBaseStat, Perso}

import scala.concurrent.ExecutionContext.Implicits.global

object TestRPG extends App {
  var id = 0

  def getid() = {
    id += 1
    id
  }

  import PlayerUIStdIn.value

  val ui = new WithUI()




  val p1 = new Perso("Bob", AnyRefBaseStat.randomInt(50, 10))
  val p2 = new Perso("Bill", AnyRefBaseStat.randomInt(50, 10))

  import ui._
  val yl = TimeLineParam(0, 50, 70)
  yl.add(p1)
  yl.add(p2)
  implicit val acImpl: ActionResolver[TimedTrait[Any], List[TimedTrait[_]]] = {
    (a: TimedTrait[_], action: Action, b: List[TimedTrait[_]]) =>
      a.value match {
        case e: Perso => e.resolve(action, b.value)
      }
  }

  for (_ <- 1 to 100) {
    yl.nextState
  }


}










