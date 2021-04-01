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



  implicit val memoXml: XmlRepCapt[Memo,MemoListIpnutR] ={
    (memo,catpeur) =>
      <div>
        <h1>
          <a class="a-title" href={s"/app/memo/${memo.id.getOrElse(0)}"}>{memo.title}</a>
        </h1>
        <div class="m-type">type :
          {memo.memoType}</div>
        <h2>content</h2><div class="m-content">{memo.memoType match {
        case MemoType.Text => memo.content
        case MemoType.Json =>
          Try {
            val l = new MemoListIpnutR(JSON.parse(memo.content).asInstanceOf[MemoListJS])
            catpeur(Some(l))
            l.xml
          } match {
            case Failure(_) => s"Erreur en traitant : ${memo.content}"
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

  implicit val memoKeyWordXml: XmlRepCapt[MemoKeywords,MemoListIpnutR] ={
    (memo,catpeur) =>
      <div>
        ---{memo.memo.xml(catpeur)}---
        <h3>tags</h3>{memo.keyWords.xml}
        <button class="btn-edit">edit</button>
        <button class="btn-save">save</button>
      </div>
  }


}




