package bon.jo.rpg
import bon.jo.rpg.BattleTimeLine.UpdateGameElement
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.BattleTimeLine._
import scala.reflect.ClassTag
trait AffectResolver[A, B ]

trait AffectResolverTPA[A, B <: TPA] extends  AffectResolver[A, B ] with AffectResolverWithResolver[A , B ]
object AffectResolver:
  trait Resolver[A,B,C <: Affect]:
    def resolveAffect(a: A, b: B)(using ui : PlayerUI) : B
      

trait AffectResolverWithResolver[A , B<: TPA ]:
  this : AffectResolver[A, B ]=>
    def resolveAffect[C <: Affect](a: A,  b: Iterable[B])(using r:  AffectResolver.Resolver[A,B,C],ui : PlayerUI,ct : C): Iterable[UpdateGameElement]=
        b.map( tpa => UpdateGameElement(tpa.id, (futureMe) =>   r.resolveAffect(a,futureMe.cast),ct.name))
      

