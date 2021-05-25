package bon.jo.rpg
import bon.jo.rpg.BattleTimeLine.UpdateGameElement
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.BattleTimeLine._
import scala.reflect.ClassTag
trait ActionResolver[A, B ]:
  def resolve(a: A, action: Action, b: Iterable[B]): Iterable[UpdateGameElement]

object ActionResolver:
  trait Resolver[A,B,C <: Action]:
    def resolve(a: A, b: B)(using ui : PlayerUI) : B
      

  trait ActionResolverWithResolver[A , B<: TPA ]:
    this : ActionResolver[A, B ]=>
      def resolve[C <: Action](a: A,  b: Iterable[B])(using r:  Resolver[A,B,C],ui : PlayerUI,ct : C): Iterable[UpdateGameElement]=
        b.map( tpa => UpdateGameElement(tpa.id, (futureMe) =>   r.resolve(a,futureMe.cast),ct.name))
      

