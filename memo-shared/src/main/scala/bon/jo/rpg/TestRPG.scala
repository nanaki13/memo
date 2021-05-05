package bon.jo.rpg

import bon.jo.rpg.Action.{ActionCtx, PlayerUIStdIn}
import bon.jo.rpg.Action.PlayerUIStdIn.value
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Perso.{PeroPero, PersoOps, WithUI}
import bon.jo.rpg.stat.{Actor, GenBaseState, Perso}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

object TestRPG extends App {
  var id = 0

  def getid() = {
    id += 1
    id
  }

  import PlayerUIStdIn.value

  val ui = new WithUI()




  val p1 = Perso("Bob", GenBaseState.randomInt(50, 10))
  val p2 = Perso("Bill", GenBaseState.randomInt(50, 10))

  import ui._
  val yl = TimeLineParam(0, 50, 70)
  yl.add(p1)
  yl.add(p2)
  implicit val acImpl: Actions[TimedTrait[Any], List[TimedTrait[_]]] = {
    (a: TimedTrait[_], action: Action, b: List[TimedTrait[_]]) =>
      a.value match {
        case e: Perso => e.resolve(action, b.value)
      }
  }

  for (_ <- 1 to 100) {
    yl.nextState
  }


}










