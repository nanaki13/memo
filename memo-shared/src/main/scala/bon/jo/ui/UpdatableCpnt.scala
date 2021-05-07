package bon.jo.ui

trait UpdatableCpnt[T] {
  def update(value: Option[T]): Unit
}
