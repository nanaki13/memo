package bon.jo.rpg.ui

import bon.jo.rpg.Action.{ActionCtx, fromStdIn}
import bon.jo.rpg.TimedTrait
import bon.jo.ui.UpdatableCpnt

import scala.concurrent.Future

object PlayerUIStdIn:
  object Value extends PlayerUI:
    type T = MessagePlayer

    override def ask(d: TimedTrait[_], cible: List[TimedTrait[_]]): Future[ActionCtx] = fromStdIn(d, cible)

    override def message(str: String, timeToDisplay: Int): Unit = println(str)

    override def message(str: String): MessagePlayer = new MessagePlayer {}

    override def clear(str: MessagePlayer): Unit = {

    }

    override type S = Any

    override def cpntMap: Any => UpdatableCpnt[Any] = e => new UpdatableCpnt[Any] {
      override def update(value: Option[Any]): Unit =
        println(value)
    }

  implicit val value: PlayerUI = Value
