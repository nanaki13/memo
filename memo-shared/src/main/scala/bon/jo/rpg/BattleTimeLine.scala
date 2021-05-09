package bon.jo.rpg

import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.PlayerUI

import scala.concurrent.{ExecutionContext, Future}


object BattleTimeLine {


  trait TimeLineOps {
    self: TimeLineParam =>


    var cnt = 0

    def update(pf: TimedTrait[_]): Unit = {

      pf.pos = pf.pos + pf.speed

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

    def updateAll = {
      timedObjs.foreach(update)

    }

    var timedObjs: List[TimedTrait[_]] = Nil

    def add[A: Timed](a: A): Unit = {
      timedObjs = timedObjs :+ a.timed
    }


    def state: Seq[(TimedTrait[_], State)] = timedObjs.map(p => (p, state(p.pos, p.speed)))

    var pause = 0

    def back(pf: TimedTrait[_]): Unit = {
      pf.pos = pf.pos - pf.speed
    }

    var resume : ()=>Unit = ()=>{}
    def nextState(implicit acImpl: ActionResolver[TimedTrait[Any], List[TimedTrait[_]]], ui: PlayerUI, ec: ExecutionContext) = {

      if (pause == 0) {
        updateAll
        val state_ = state
        val toAsk: Seq[() => Future[Unit]] = state_.filter(_._2 == State.ChooseAction).map {
          case (pos, state) =>
            () => val message = ui.message(s"selectionner l'action pour ${pos.simpleName}")
           // pause += 1
            ui.ask(pos, timedObjs).map { act =>
              ui.clear(message.asInstanceOf[ui.T])
              ui.message("ChoosdeAction",1000)
              pos.action = act
            }
        }
        if(toAsk.nonEmpty){
          pause+=1
          println("run root Seq")
          PlayerUI.runSeq(toAsk).foreach(_=>{
            println("run Seq res")
            pause-=1
            if(pause == 0){
              resume()
            }
          })
        }else{
          state_.foreach {
            case (pos: TimedTrait[Any], state) =>
              val cible = pos.action.cible: List[TimedTrait[_]]
              state match {
                case State.BeforeChooseAction => //ui.message("BeforeChooseAction")
//                case State.ChooseAction if (pause <= 0) => {
//                  val message = ui.message(s"selectionner l'action pour ${pos.simpleName}")
//                  pause += 1
//                  ui.ask(pos, timedObjs).foreach { act =>
//                    ui.clear(message.asInstanceOf[ui.T])
//                    ui.message("ChoosdeAction",1000)
//                    pos.action = act
//                    pause -= 1
//                    if(pause == 0){
//                      resume()
//                    }
//                  }
//                }
//                case State.ChooseAction if (pause > 0) => {
//                  back(pos)
//                }
                case State.BeforeResolveAction => // ui.message("BeforeResolveAction")
                case State.ResolveAction =>
                  ui.message("_____ResolveAction____",5000)
                  pos.pos = 0
                  ui.message(s"${pos.simpleName} fait ${pos.action.action} sur ${pos.action.cible.map(_.simpleName).mkString(", ")}",5000)
                  pos.resolve(pos.action.action, cible)
                case State.NoState =>
              }
            case _ => println("PAUSE")
          }

        }

      }else{
        println("PAUSE")
      }

    }

  }

  case class TimeLineParam(start: Int, chooseAction: Int, action: Int) extends TimeLineOps


}
