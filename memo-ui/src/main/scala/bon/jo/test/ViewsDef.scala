package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords}
import bon.jo.test.XmlRep._
import org.scalajs.dom.console

object ViewsDef {
  def apply(implicit idMemop: Id[Memo], idKp: Id[KeyWord], idMemoKwp: Id[MemoKeywords]): ViewsDef = new ViewsDef(idMemop, idKp, idMemoKwp)
}

class ViewsDef(
                idMemop: Id[Memo],
                idKp: Id[KeyWord], idMemoKwp: Id[MemoKeywords]) {
  implicit val idMemo: Id[Memo] = idMemop
  implicit val idK: Id[KeyWord] = idKp
  implicit val idMemoKw: Id[MemoKeywords] = idMemoKwp

  console.log(idMemo, idK, idMemoKw)
  implicit val memoXml: IdXmlRep[Memo] = XmlRep[bon.jo.memo.Entities.Memo] {
    memo =>
      <div>
        <h1>
          {memo.title}
        </h1>{memo.content}
      </div>
  }
  implicit val keyWord: IdXmlRep[KeyWord] = XmlRep[bon.jo.memo.Entities.KeyWord] {
    memo => <div>
      {memo.value}
    </div>
  }

  implicit val memoKeyWordXml: IdXmlRep[MemoKeywords] = XmlRep[bon.jo.memo.Entities.MemoKeywords] {
    memo =>
      <div>
        {memo.memo.xml}{memo.keyWords.xml}
      </div>
  }


}




