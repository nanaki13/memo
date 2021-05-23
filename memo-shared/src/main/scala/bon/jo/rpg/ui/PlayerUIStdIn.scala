package bon.jo.rpg.ui

import bon.jo.rpg.Action.{ActionCtx, fromStdIn}
import bon.jo.rpg.TimedTrait
import bon.jo.rpg.stat.GameElement
import bon.jo.ui.UpdatableCpnt

import scala.concurrent.Future
import bon.jo.rpg.stat.Perso

object PlayerUIStdIn:
  object Value extends PlayerUI:
    type T = MessagePlayer

    override def ask(d: TimedTrait[GameElement], cible: List[TimedTrait[GameElement]]): Future[ActionCtx[GameElement]] = fromStdIn(d, cible)

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
