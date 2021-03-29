package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.XmlRep._
import org.scalajs.dom.console

import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  def apply(implicit idMemop: Id[Memo], idKp: Id[KeyWord], idMemoKwp: Id[MemoKeywords]): ViewsDef = new ViewsDef(idMemop, idKp, idMemoKwp)
}

class ViewsDef(
                idMemop: Id[Memo],
                idKp: Id[KeyWord], idMemoKwp: Id[MemoKeywords]) {
  implicit val idMemo: Id[Memo] = idMemop
  implicit val idK: Id[KeyWord] = idKp
  implicit val idMemoKw: Id[MemoKeywords] = idMemoKwp


  implicit val memoXml: IdXmlRep[Memo] = XmlRep[bon.jo.memo.Entities.Memo] {
    memo =>
      <div>
        <h1>
          {memo.title}
        </h1>
        <div>type :
          {memo.memoType}</div>
        <h2>content</h2><div>{memo.memoType match {
        case MemoType.Text => memo.content
        case MemoType.Json =>
          Try {
            (new MemoListIpnutR(JSON.parse(memo.content).asInstanceOf[MemoListJS])).xml
          } match {
            case Failure(exception) => s"Erreur en traitant : ${memo.content}"
            case Success(value) =>  value
          }
      }
        }</div>
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
        {memo.memo.xml}
        <h3>tags</h3>{memo.keyWords.xml}
      </div>
  }


}




