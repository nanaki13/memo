package bon.jo.rpg

import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.PlayerUI

import scala.concurrent.{ExecutionContext, Future}
import bon.jo.rpg.stat.{ GameElement}


object BattleTimeLine:


  type TP[A] = TimedTrait[A]
  type LTP[A] = List[TP[A]]
  type ITP[A] = Iterable[TP[A]]
  type TPA = TP[GameElement]
  type LTPA = LTP[GameElement]
  type ITPA = ITP[GameElement]
  enum NextStateResult(change : List[() => GameElement]):
    case  NextStateResultFast( fast : ITP[GameElement]) extends NextStateResult(Nil)
    case  NextStateResultAsking( fast : ITP[GameElement], ask :  Future[ITP[GameElement]])  extends NextStateResult(Nil)


 // def apply(using TimeLineParam) =
  object TimeLineOps{
    class i()(using params :  TimeLineParam,t : Timed[GameElement]) extends TimeLineOps
    def apply()(using params :  TimeLineParam,t :  Timed[GameElement]) : TimeLineOps = i()
  }
  trait TimeLineOps(using val params :  TimeLineParam)(using Timed[GameElement]):
    

 
    var pause = 0
    var uiUpdate : ITPA => Unit = _ => ()
    var timedObjs: LTPA = Nil
    var cnt = 0
    def stop(): Unit =
      timedObjs = Nil
      pause = 0


    def update(pf: TPA): TPA =

      pf.withPos(pf.pos + pf.speed)



    def state(pos: Int, spooed: Int): State =
      pos match
        case i if (i < params.chooseAction) => State.BeforeChooseAction
        case i if (i >= params.chooseAction && (i < params.chooseAction + spooed)) => State.ChooseAction
        case i if (i < params.action) => State.BeforeResolveAction
        case i if (i >= params.action) => State.ResolveAction
        case _ => State.NoState

    def updateAll(a :LTPA ): LTPA =
      a.map(update)




    def add(a: GameElement): Unit =
      timedObjs = timedObjs :+ a.timed





    def state(e : LTPA): Seq[(TPA, State)] = e.map(p => (p, state(p.pos, p.speed)))



    var resume : ()=>Unit = ()=>{}


    def nextState(timedObjs : LTPA)(implicit acImpl: ActionResolver[TPA,
      LTPA], ui: PlayerUI, ec: ExecutionContext): NextStateResult =

      if pause == 0 then
        val state_ =  state(updateAll(timedObjs))
        uiUpdate(state_.map(_._1))
        val fast = state_.filter(_._2 != State.ChooseAction).map{
           (pos, state) =>
            val cible = pos.action.cible: LTPA
            state match
              case State.BeforeChooseAction => pos
              case State.BeforeResolveAction => pos
              case State.ResolveAction =>
                val message = s"${pos.simpleName} fait ${pos.action.action} ${if pos.action.cible.nonEmpty then
                  s"sur ${pos.action.cible.map(_.simpleName).mkString(", ")}"}"

                ui.message(s"${pos.simpleName} fait ${pos.action.action} sur ${pos.action.cible.map(_.simpleName).mkString(", ")}",5000)
                pos.asInstanceOf[TPA].resolve(pos.action.action, cible)(acImpl)
                pos.withPos(0)
              case State.NoState =>pos//
              case _ => ???
        }
        if  state_.count(_._2 == State.ChooseAction)>0 then
          val toAsk: Seq[() => Future[TPA]] = state_.filter(_._2 == State.ChooseAction).map {
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
          NextStateResult.NextStateResultAsking(fast,PlayerUI.runSeq(toAsk) )
        else
           NextStateResult.NextStateResultFast(fast)
      else
         NextStateResult.NextStateResultFast(timedObjs)



  case class TimeLineParam(start: Int, chooseAction: Int, action: Int)


