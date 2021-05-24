package bon.jo.rpg
import bon.jo.rpg.BattleTimeLine.UpdateGameElement
trait ActionResolver[A, B ]:
  def resolve(a: A, action: Action, b: List[B]): List[UpdateGameElement]
