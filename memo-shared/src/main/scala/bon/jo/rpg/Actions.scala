package bon.jo.rpg

trait Actions[A, B] {
  def resolve(a: A, action: Action, b: B): Unit
}
