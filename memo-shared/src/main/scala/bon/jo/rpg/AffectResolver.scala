package bon.jo.rpg
import bon.jo.rpg.BattleTimeLine.UpdateGameElement
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.BattleTimeLine._
import scala.reflect.ClassTag


object AffectResolver:
  type AffectResolverType
  trait Resolver[A,B,C <: Affect]:
    def resolveAffect(a: A, b: B) : PlayerUI.UI[B]
      

trait AffectResolver[A , B<: TPA ]:
  def resolveAffect[C <: Affect](a: A,  b: Iterable[B])(using r:  AffectResolver.Resolver[A,B,C],ct : C): PlayerUI.UI[Iterable[UpdateGameElement]] =
    b.map( tpa => UpdateGameElement(tpa.id, (futureMe) =>   r.resolveAffect(a,futureMe.cast),ct.name))
      

