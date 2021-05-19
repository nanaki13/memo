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
  def runSeq(toAsk: Seq[() => Future[Unit]])(implicit ec: ExecutionContext): Future[Unit] = {

    if (toAsk.isEmpty) {

      Future.successful(())
    } else {
      toAsk.head().flatMap {
        _ => runSeq(toAsk.tail)
      }
    }
  }
}