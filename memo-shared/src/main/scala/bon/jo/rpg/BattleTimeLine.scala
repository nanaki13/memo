package bon.jo.rpg

import bon.jo.rpg.DoActionTrait.WithAction
import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.PlayerUI

import scala.concurrent.{ExecutionContext, Future}
import bon.jo.rpg.stat.{ GameElement}
import BattleTimeLine.UpdateGameElement
import BattleTimeLine._
object BattleTimeLine:


  type TP[A] = TimedTrait[A]
  type LTP[A] = List[TP[A]]
  type ITP[A] = Iterable[TP[A]]
  type TPA = TP[GameElement]
  type LTPA = LTP[GameElement]
  type ITPA = ITP[GameElement]
  opaque type UpdateGameElement = (Int,TPA=>TPA)
  object UpdateGameElement:
    def apply(id : Int,tr : TPA=>TPA):UpdateGameElement = (id,tr)
  extension (a : UpdateGameElement)
    def idEl = a._1
    def transfrom(b : TPA) = a._2(b)
  enum NextStateResult(change : List[UpdateGameElement]):
    case  NextStateResultFast( fast : ITP[GameElement],change : List[UpdateGameElement]) extends NextStateResult(change)
    case  NextStateResultAsking( fast : ITP[GameElement], ask :  Future[ITP[GameElement]],change : List[UpdateGameElement])  extends NextStateResult(change)


 // def apply(using TimeLineParam) =
  object TimeLineOps{
    class i()(using params :  TimeLineParam,t : Timed[GameElement]) extends TimeLineOps
    def apply()(using params :  TimeLineParam,t :  Timed[GameElement]) : TimeLineOps = i()
  }
  trait TimeLineOps(using val params :  TimeLineParam)(using Timed[GameElement]):
    println(s"inti with : $params")

 
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

    def dochange(all : ITPA,update : List[UpdateGameElement])(using  ActionResolver[TPA,
      TPA],  PlayerUI, ExecutionContext): ITPA = 
      val mapById = all.map(e => e.id -> e).toMap
      val updatedMap = update.map{ e => e.transfrom(mapById(e.idEl))}.map(e => e.id -> e).toMap
      all.map{
        e =>
          updatedMap.getOrElse(e.id,e)
      }
        
      
        

    def doStep(using  ActionResolver[TPA,
      TPA],  PlayerUI,  ExecutionContext) = 
      nextState(timedObjs) match 
      case NextStateResult.NextStateResultFast(fast,change) => timedObjs = dochange(fast,change).toList.sorted(ordering)
      case NextStateResult.NextStateResultAsking(fast, ask,change) => ask foreach{
        askWithResult =>
          timedObjs =  dochange(fast++askWithResult,change).toList.sorted(ordering)
      }

    var ordering :  Ordering[TimedTrait[GameElement]] = _
    def evaluateCurrentOrdering() : Ordering[TimedTrait[GameElement]] =
      val idToOrder = timedObjs.map(_.id).zipWithIndex.toMap
      (a,b) => idToOrder(a.id).compare(b.id)
    def add(a: GameElement): Unit =
      timedObjs = timedObjs :+ a.timed
      ordering = evaluateCurrentOrdering()





    def state(e : LTPA): Seq[(TPA, State)] = e.map(p => (p, state(p.pos, p.speed)))



    var resume : ()=>Unit = ()=>{}


    def nextState(timedObjs : LTPA)(using z : ActionResolver[TPA,
      TPA], ui: PlayerUI, ec: ExecutionContext): NextStateResult =

      if pause == 0 then
        val state_ =  state(updateAll(timedObjs))
        uiUpdate(state_.map(_._1))
        val fast = state_.filter(_._2 != State.ChooseAction).map{
           (pos, state) =>
            val cible = pos.action.cible: LTPA
            state match
              case State.BeforeChooseAction => (pos,Nil)
              case State.BeforeResolveAction => (pos,Nil)
              case State.ResolveAction =>
                val message = s"${pos.simpleName} fait ${pos.action.action} ${if pos.action.cible.nonEmpty then
                  s"sur ${pos.action.cible.map(_.simpleName).mkString(", ")}"}"

                ui.message(s"${pos.simpleName} fait ${pos.action.action} sur ${pos.action.cible.map(_.simpleName).mkString(", ")}",5000)
                val updayedCible =  pos.resolve(pos.action.action, cible)
                (pos.withPos(0),updayedCible)
              case State.NoState =>(pos,Nil)
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
          NextStateResult.NextStateResultAsking(fast.map(_._1),PlayerUI.runSeq(toAsk),fast.flatMap(_._2).toList )
        else
           NextStateResult.NextStateResultFast(fast.map(_._1),fast.flatMap(_._2).toList)
      else
         NextStateResult.NextStateResultFast(timedObjs,Nil)



  case class TimeLineParam(start: Int, chooseAction: Int, action: Int)


