package bon.jo.rpg

import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.PlayerUI

import scala.concurrent.{ExecutionContext, Future}


object BattleTimeLine:

  sealed trait NextStateResult
  case class NextStateResultFast( fast : Iterable[TimedTrait[_]]) extends NextStateResult
  case class NextStateResultAsking( fast : Iterable[TimedTrait[_]], ask :  Future[Iterable[TimedTrait[_]]]) extends NextStateResult

  trait TimeLineOps:
    self: TimeLineParam =>


    var cnt = 0

    def update(pf: TimedTrait[_]): TimedTrait[_] =

      pf.withPos(pf.pos + pf.speed)


    var uiUpdate : Iterable[TimedTrait[_]] => Unit = _ => ()
    def state(pos: Int, spooed: Int): State =
      pos match
        case i if (i < chooseAction) => State.BeforeChooseAction
        case i if (i >= chooseAction && (i < chooseAction + spooed)) => State.ChooseAction
        case i if (i < action) => State.BeforeResolveAction
        case i if (i >= action) => State.ResolveAction
        case _ => State.NoState

    def updateAll(a : List[TimedTrait[_]] ): List[TimedTrait[_]] =
      a.map(update)


    var timedObjs: List[TimedTrait[_]] = Nil

    def add[A: Timed](a: A): Unit =
      timedObjs = timedObjs :+ a.timed


    def stop(): Unit =
      timedObjs = Nil
      pause = 0


    def state(e : List[TimedTrait[_]]): Seq[(TimedTrait[_], State)] = e.map(p => (p, state(p.pos, p.speed)))

    var pause = 0


    type T[A] = List[TimedTrait[A]]

    var resume : ()=>Unit = ()=>{}

    def nextState(timedObjs : List[TimedTrait[_]])(implicit acImpl: ActionResolver[TimedTrait[Any],
      List[TimedTrait[_]]], ui: PlayerUI, ec: ExecutionContext): NextStateResult =

      if pause == 0 then
        val state_ =  state(updateAll(timedObjs))
        uiUpdate(state_.map(_._1))
        val fast = state_.filter(_._2 != State.ChooseAction).map{
           (pos, state) =>
            val cible = pos.action.cible: List[TimedTrait[_]]
            state match
              case State.BeforeChooseAction => pos//ui.message("BeforeChooseAction")
              case State.BeforeResolveAction => pos//// ui.message("BeforeResolveAction")
              case State.ResolveAction =>
                ui.message(s"${pos.simpleName} fait ${pos.action.action} sur ${pos.action.cible.map(_.simpleName).mkString(", ")}",5000)
                pos.asInstanceOf[bon.jo.rpg.TimedTrait[Any]].resolve(pos.action.action, cible)(acImpl)
                pos.withPos(0)
              case State.NoState =>pos//
              case _ => ???
        }
        if  state_.count(_._2 == State.ChooseAction)>0 then
          val toAsk: Seq[() => Future[TimedTrait[_]]] = state_.filter(_._2 == State.ChooseAction).map {
             (pos,_) =>
              () => {
                val message = ui.message(s"selectionner l'action pour ${pos.simpleName}")
                pause += 1
                ui.ask(pos, timedObjs).map { act =>
                  ui.clear(message.asInstanceOf[ui.T])

                  pause -= 1
                  val ret = pos.withAction(act)
                  if pause == 0 then
                    resume()

                  ret
                }
              }

          }
          NextStateResultAsking(fast,PlayerUI.runSeq(toAsk) )
        else
          NextStateResultFast(fast)
      else
        NextStateResultFast(timedObjs)



  case class TimeLineParam(start: Int, chooseAction: Int, action: Int) extends TimeLineOps


