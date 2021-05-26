package bon.jo.rpg


import bon.jo.rpg.TimedTrait._
import bon.jo.rpg.ui.PlayerUI

import scala.concurrent.{ExecutionContext, Future}
import bon.jo.rpg.stat.{ GameElement}
import BattleTimeLine.UpdateGameElement
import BattleTimeLine._
import bon.jo.rpg.stat.GameId
import bon.jo.rpg.resolve.PersoResolveContext._
import bon.jo.rpg.stat.Perso.PersoOps
import bon.jo.rpg.Commande
import bon.jo.rpg.stat.Perso
object BattleTimeLine:


  type TP[A] = TimedTrait[A]
  type LTP[A] = List[TP[A]]
  type ITP[A] = Iterable[TP[A]]
  type TPA = TP[GameElement]
  type LTPA = LTP[GameElement]
  type ITPA = ITP[GameElement]
  type Res = CommandeResolver.Dispatcher[ TimedTrait[Perso], TimedTrait[GameElement]] 
  opaque type UpdateGameElement = (GameId.ID,TPA=>TPA,String)
  object UpdateGameElement:
    def apply(id : GameId.ID,tr : TPA=>TPA,name : String):UpdateGameElement = (id,tr,name)
  extension (a : UpdateGameElement)
    def idEl = a._1
    def transfrom(b : TPA) = a._2(b)
    def name = a._3
    def andThen(b : UpdateGameElement) : UpdateGameElement=
      UpdateGameElement(a.idEl, a._2.andThen( b._2), s"${a.name} puis ${b.name} " ) 
    def andThenList(b : List[UpdateGameElement])  : UpdateGameElement=
      (a +: b).reduceLeft( _ andThen _)
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

    def dochange(all : ITPA,update : List[UpdateGameElement])(using ui :   PlayerUI): ITPA = 
      val mapById = all.map(e => e.id -> e).toMap
      val mapTrById = (update.groupMapReduce(_._1)(e => e)(_ andThen _ )).values
      val updatedMap = mapTrById.map{ e => 
        ui.message(s"application de ${e.name} ",0)
        e.transfrom(mapById(e.idEl))
        
        }.map(e => e.id -> e).toMap
      all.map{
        e =>
          updatedMap.getOrElse(e.id,e)
      }
        
      
  

    def doStep(using    PlayerUI,  ExecutionContext,CommandeResolver.Dispatcher[TP[Perso],TPA] ) = 
      nextState(timedObjs) match 
      case NextStateResult.NextStateResultFast(fast,change) => timedObjs = dochange(fast,change).toList.sorted
      case NextStateResult.NextStateResultAsking(fast, ask,change) => ask foreach{
        askWithResult =>
          timedObjs =  dochange(fast++askWithResult,change).toList.sorted
      }


    def add(a: GameElement): Unit =
      timedObjs = timedObjs :+ a.timed
    





    def state(e : LTPA): Seq[(TPA, State)] = e.map(p => (p, state(p.pos, p.speed)))



    var resume : ()=>Unit = ()=>{}


    def nextState(timedObjs : LTPA)(using z : CommandeResolver.Dispatcher[TP[Perso],TPA], ui: PlayerUI, ec: ExecutionContext): NextStateResult =

      if pause == 0 then
        val state_ :  Seq[(TPA, State)]=  state(updateAll(timedObjs))
        def m : Map[GameId.ID,TPA] = state_.map(_._1).map(( a:TPA ) => a.id -> a).toMap
        uiUpdate(state_.map(_._1))
        val cpntMap : Option[Map[GameId.ID,TimedTrait[GameElement]]] = if state_.count(_._2 == State.ResolveAction) > 0 then
          Some(m )
        else
          None
        val fast = state_.filter(_._2 != State.ChooseAction).map{
           (pos, state) =>
            val cible = pos.commandeCtx.cible: Iterable[bon.jo.rpg.stat.GameId.ID]
            def trueCible = cible.map(cpntMap.get(_))
            state match
              case State.BeforeChooseAction => (pos,Nil)
              case State.BeforeResolveAction => (pos,Nil)
              case State.ResolveAction =>
                val cpnts = trueCible

                val message = s"${pos.simpleName} fait ${pos.commandeCtx.commande} ${if pos.commandeCtx.cible.nonEmpty then
                  s"sur ${cpnts.map(_.simpleName).mkString(", ")}"}"

                ui.message(message,5000)
              
                val updayedCible =  z.resolveCommand(pos.cast, pos.commandeCtx.commande,trueCible)
                (pos.withPos(0),updayedCible)
              case State.NoState =>(pos,Nil)
              case _ => ???
        }
        if  state_.count(_._2 == State.ChooseAction)>0 then
          val toAsk: Seq[() => Future[TPA]] = state_.filter(_._2 == State.ChooseAction).map {
             (pos,_) =>
              () => {
                val message = ui.message(s"selectionner la commande pour ${pos.simpleName}")
                pause += 1
                ui.ask(pos, timedObjs).map { act =>
                  ui.clear(message.asInstanceOf[ui.T])

                  pause -= 1
                  val ret = pos.withCommandeCtx(act)
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


