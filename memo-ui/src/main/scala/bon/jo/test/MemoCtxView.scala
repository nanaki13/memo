package bon.jo.test

import bon.jo.memo.Entities
import bon.jo.memo.Entities.MemoType
import bon.jo.test.SimpleView.{i, s, ta}
import org.scalajs.dom.html.{Input, Select, TextArea}
import bon.jo.html.DomShell.ExtendedElement
import bon.jo.html.HtmlEventDef.ExH

import scala.scalajs.js.JSON

class MemoCtxView {


  val memoList: MemoListView = new MemoListView()

  val tInput: Input = i
  val contentInput: TextArea = ta
  val memoType: Select = s[MemoType]


  def newMemo: Entities.Memo = {
    val mt = MemoType(memoType.value)
    val ret = mt match {
      case MemoType.Text => new Entities.Memo(tInput.value, contentInput.value, MemoType(memoType.value))
      case MemoType.Json => new Entities.Memo(tInput.value, JSON.stringify(memoList.read().pure()), MemoType(memoType.value))
    }

    ret

  }

  def makeSwitchView(): Unit = {
    memoType.$change { _ => {
      MemoType(memoType.value) match {
        case MemoType.Text =>
          contentInput.show(true)
          memoList.html.show(false)
        case MemoType.Json =>
          contentInput.show(false)
          memoList.html.show(true)
      }
    }

    }
  }
}
