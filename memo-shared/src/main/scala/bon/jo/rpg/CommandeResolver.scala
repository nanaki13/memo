package bon.jo.rpg
import bon.jo.rpg.BattleTimeLine.UpdateGameElement
import bon.jo.rpg.Commande
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.BattleTimeLine._
import scala.reflect.ClassTag

object CommandeResolver:

      

  trait Dispatcher[A , B<: TPA ]:
      def resolveCommand(a: A,commande : Commande,  b: Iterable[B]): Iterable[UpdateGameElement]
      
      
      

