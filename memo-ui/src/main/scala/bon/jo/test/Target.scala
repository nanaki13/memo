package bon.jo.test

object Target {

  case object MemoCreation extends Target

  case class ReadMemo(id: Int) extends Target

  case object KeyWordK extends Target

  case object _404 extends Target

}

sealed trait Target extends Product