package bon.jo.test

import bon.jo.memo.Dao.Id
import bon.jo.memo.Entities.{KeyWord, Memo, MemoKeywords, MemoType}
import bon.jo.test.XmlRep._
import org.scalajs.dom.console

import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}

object ViewsDef {
  def apply(): ViewsDef = new ViewsDef
}

class ViewsDef() {



  implicit val memoXml: XmlRep[Memo] ={
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
  implicit val keyWord: XmlRep[KeyWord] = {
    memo => <div>
      {memo.value}
    </div>
  }

  implicit val memoKeyWordXml: XmlRep[MemoKeywords] ={
    memo =>
      <div>
        {memo.memo.xml}
        <h3>tags</h3>{memo.keyWords.xml}
      </div>
  }


}




