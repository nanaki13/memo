package bon.jo.rpg.ui

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.TimedTrait
import bon.jo.ui.UpdatableCpnt

import scala.concurrent.{ExecutionContext, Future}

trait PlayerUI extends PlayerMessage {

  type S

  def cpntMap: S => UpdatableCpnt[S]

  def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx]

}

object PlayerUI {
  def runSeq[A](toAsk: Seq[() => Future[A]],res : List[A] = Nil)(implicit ec: ExecutionContext): Future[ List[A]] = {

    if (toAsk.isEmpty) {
      Future.successful((res))
    } else {
      toAsk.head().flatMap {
        e => runSeq(toAsk.tail,res :+ e)
      }
    }
  }
}