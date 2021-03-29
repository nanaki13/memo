package bon.jo.test

import bon.jo.memo.Entities.{MemoKeywords, MemoType}
import bon.jo.memo.{BaseRoute, Entities}
import org.scalajs.dom.console

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.language.dynamics
object Daos {

  trait MemoJs extends js.Object{
    val id: Int
    val title: String
    val content: String
    val memoType: String
  }
  def o[A](option: Option[A])( f: A => js.Any): js.Any = option match {
    case Some(value) => f(value)
    case None => js.undefined
  }
  object MemoJs{
    def apply(m: Entities.Memo): MemoJs = js.Dynamic.literal(
      id =o(m.id)(a => a),
      content = m.content,
      title = m.title,
      memoType = m.memoType.toString
    ).asInstanceOf[MemoJs]
  }
  trait KeyWordJs extends js.Object{
    val id: Int
    val value: String
  }
  trait MemoKeywordsJs extends js.Object{
    val tmp : MemoKeywords
    val keyWords :  js.Array[KeyWordJs]
    val memo :  MemoJs
    val value: String
  }
  object MemoKeywordsJs{
    def apply(m: Entities.MemoKeywords): MemoKeywordsJs = {
     val ret =  js.Dynamic.literal(
        memo = MemoJs(m.memo),
        keyWords = m.keyWords.map(KeyWordJs.apply).toJSArray,
      ).asInstanceOf[MemoKeywordsJs]
      console.log(ret)
      ret
    }
  }
  object KeyWordJs{
    def apply(m: Entities.KeyWord): KeyWordJs = js.Dynamic.literal(
      id =o(m.id)(a => a),
      value = m.value,
    ).asInstanceOf[KeyWordJs]
  }

  object memoDao extends HttpDao[Entities.Memo, Int,MemoJs] {
    override val writer: Entities.Memo => MemoJs =  MemoJs.apply
    override val readerOne: MemoJs => Entities.Memo =js => Entities.Memo(Some(js.id),js.title,js.content,MemoType(js.memoType))
    override val url: String = s"http://localhost:8080/${BaseRoute.memoRoute}"
  }
  object keyWordDao extends HttpDao[Entities.KeyWord, Int,KeyWordJs] {
    override val writer: Entities.KeyWord => KeyWordJs =  KeyWordJs.apply
    override val readerOne: KeyWordJs => Entities.KeyWord =js => Entities.KeyWord(Some(js.id),js.value)
    override val url: String = s"http://localhost:8080/${BaseRoute.keywordRoute}"
  }

  object memoKeyWord extends HttpDao[Entities.MemoKeywords, Int,MemoKeywordsJs] {
    override val writer: Entities.MemoKeywords => MemoKeywordsJs =  MemoKeywordsJs.apply
    override val readerOne: MemoKeywordsJs => Entities.MemoKeywords =js => Entities.MemoKeywords(memoDao.readerOne(js.memo),keyWordDao.readerMany(js.keyWords).toSet)
    override val url: String = s"http://localhost:8080/${BaseRoute.memoKeyWordRoute}"
  }


}

