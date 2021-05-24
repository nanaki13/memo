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
    def register(id: Int, g: bon.jo.rpg.stat.GameElement): Unit=
      ()
    override def ask(d: TimedTrait[GameElement], cible: List[TimedTrait[GameElement]]): Future[ActionCtx[GameElement]] = fromStdIn(d, cible)

    override def message(str: String, timeToDisplay: Int): Unit = println(str)

    override def message(str: String): MessagePlayer = new MessagePlayer {}

    override def clear(str: MessagePlayer): Unit = {

    }

 

    override def cpntMap: Int => UpdatableCpnt[GameElement] = e => new UpdatableCpnt[GameElement] {
      override def update(value: Option[GameElement]): Unit =
        println(value)
    }

  implicit val value: PlayerUI = Value
