package bon.jo.rpg
import bon.jo.rpg.BattleTimeLine._
trait DoActionTrait[A]:
  def value: A

  def resolve[B](action: Action, b: List[B])(implicit acImpl: ActionResolver[A, B]): List[UpdateGameElement] = acImpl.resolve(value, action, b)

object DoActionTrait {
  implicit class WithAction[A](val value: A) extends DoActionTrait[A] {
    // def resolveAny(action: Action, b: Any)(implicit acImpl : Actions[Any,Any]): Unit = acImpl.resolve(a, action, b)
    //   def resolve[B](action: Action,b : B)(implicit acImpl : Actions[A,B]) = acImpl.resolve(value,action,b)
  }
}