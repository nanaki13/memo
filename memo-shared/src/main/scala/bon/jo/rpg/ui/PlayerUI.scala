package bon.jo.rpg.ui

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.TimedTrait
import bon.jo.rpg.stat.GameElement
import bon.jo.ui.UpdatableCpnt

import scala.concurrent.{ExecutionContext, Future}


trait PlayerUI extends PlayerMessage:

  type S

  def cpntMap: S => UpdatableCpnt[S]

  def ask(d: TimedTrait[GameElement], cible: List[TimedTrait[GameElement]]): Future[ActionCtx[GameElement]]


object PlayerUI {
  def runSeq[A](toAsk: Seq[() => Future[A]],res : List[A] = Nil)(implicit ec: ExecutionContext): Future[ List[A]] =

    if toAsk.isEmpty then
      Future.successful((res))
    else
      toAsk.head().flatMap {
        e => runSeq(toAsk.tail,res :+ e)
      }
}