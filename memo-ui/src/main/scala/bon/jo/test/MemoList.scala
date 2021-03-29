package bon.jo.test

import scala.scalajs.js


class MemoList( val elements : List[ListElement])  extends MemoListJS  {
  def this(a : MemoListJS)={
    this(a.elements.map(new ListElement(_)))
  }
}
trait MemoListJS extends js.Object{
  val elements : List[ListElementJS]
}
trait ListElementJS extends js.Object{
  val content : String
  val checked : Boolean
}
class ListElement(val content : String,val checked : Boolean) extends ListElementJS {
  def this(a : ListElementJS)={
    this(a.content,a.checked)
  }
}
