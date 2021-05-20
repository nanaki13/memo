package bon.jo.rpg

import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.PlayerUI

import scala.concurrent.{ExecutionContext, Future}


object BattleTimeLine {


  trait TimeLineOps {
    self: TimeLineParam =>


    var cnt = 0

    def update(pf: TimedTrait[_]): TimedTrait[_] = {

      pf.withPos(pf.pos + pf.speed)

    }

    def state(pos: Int, spooed: Int): State = {
      pos match {
        case i if (i < chooseAction) => State.BeforeChooseAction
        case i if (i >= chooseAction && (i < chooseAction + spooed)) => State.ChooseAction
        case i if (i < action) => State.BeforeResolveAction
        case i if (i >= action) => State.ResolveAction
        case _ => State.NoState
      }
    }

    def updateAll(a : List[TimedTrait[_]] ): List[TimedTrait[_]] = {
      a.map(update)

    }

    var timedObjs: List[TimedTrait[_]] = Nil

    def add[A: Timed](a: A): Unit = {
      timedObjs = timedObjs :+ a.timed
    }


      def stop(): Unit = {
        timedObjs = Nil
        pause = 0
      }


    def state(e : List[TimedTrait[_]]): Seq[(TimedTrait[_], State)] = e.map(p => (p, state(p.pos, p.speed)))

    var pause = 0


    type T[A] = List[TimedTrait[A]]

    var resume : ()=>Unit = ()=>{}


    def nextState(timedObjs : List[TimedTrait[_]])(implicit acImpl: ActionResolver[TimedTrait[Any],
      List[TimedTrait[_]]], ui: PlayerUI, ec: ExecutionContext): Future[List[TimedTrait[_]]] = {

      if (pause == 0) {
        val state_ =  state(updateAll(timedObjs))
        val toAsk: Seq[() => Future[TimedTrait[_]]] = state_.map {
          case (pos, State.ChooseAction) =>
            () => {
              val message = ui.message(s"selectionner l'action pour ${pos.simpleName}")
              pause += 1
              ui.ask(pos, timedObjs).map { act =>
                ui.clear(message.asInstanceOf[ui.T])

                pause -= 1
                val ret = pos.withAction(act)
                if(pause == 0){
                  resume()
                }

                ret
              }
            }
          case (pos: TimedTrait[Any], state) =>
            val cible = pos.action.cible: List[TimedTrait[_]]
            state match {
              case State.BeforeChooseAction => ()=> Future.successful(pos)//ui.message("BeforeChooseAction")
              case State.BeforeResolveAction => ()=> Future.successful(pos)//// ui.message("BeforeResolveAction")
              case State.ResolveAction =>


                ui.message(s"${pos.simpleName} fait ${pos.action.action} sur ${pos.action.cible.map(_.simpleName).mkString(", ")}",5000)
                pos.resolve(pos.action.action, cible)
                ()=> Future.successful(pos.withPos(0))
              case State.NoState =>()=> Future.successful(pos)//
            }
         // case _ => println("PAUSE");
        }
        PlayerUI.runSeq(toAsk)
      }else{
        Future.successful(timedObjs)
      }

    }

  }

  case class TimeLineParam(start: Int, chooseAction: Int, action: Int) extends TimeLineOps


}
