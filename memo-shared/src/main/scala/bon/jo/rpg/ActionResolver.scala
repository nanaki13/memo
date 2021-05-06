package bon.jo.rpg

trait ActionResolver[A, B] {
  def resolve(a: A, action: Action, b: B): Unit
}
