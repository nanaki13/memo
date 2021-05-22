package bon.jo.rpg

import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.stat.Perso.{PeroPero, WithUI}
import bon.jo.rpg.stat.{AnyRefBaseStat, Perso}
import bon.jo.rpg.ui.{PlayerUI, PlayerUIStdIn}

import scala.concurrent.ExecutionContext.Implicits.global

object TestRPG extends App:
  var id = 0

  def getid() =
    id += 1
    id

  implicit val v: PlayerUI = PlayerUIStdIn.value

  val ui = new WithUI()


  val p1 = new Perso(1, "Bob", "Un bon gars", AnyRefBaseStat.randomInt(50, 10))
  val p2 = new Perso(2, "Bill", "Un bon gars", AnyRefBaseStat.randomInt(50, 10))


  import ui.value

  val yl = TimeLineParam(0, 50, 70)
  yl.add(p1)
  yl.add(p2)
  implicit val acImpl: ActionResolver[TimedTrait[Any], List[TimedTrait[_]]] = new ActionResolver[TimedTrait[Any], List[TimedTrait[_]]]{
    override def resolve(a: TimedTrait[Any], action: Action, b: List[TimedTrait[_]]): Unit =
      a.value match
        case e: Perso => e.resolve(action, b.value)(ui.value)
        case _ =>
  }


  for _ <- 1 to 100 do


    yl.nextState(yl.timedObjs)












