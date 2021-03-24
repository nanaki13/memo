package bon.jo.memo

object Entities {

  case class Memo(id: Option[Int], title: String, content: String){
    def this(title : String,content : String) = this(None,title,content)
  }

  case class KeyWord(id: Option[Int], value: String)

  case class MemoKeywordRel(memo: Int, keyWord: Int)

  case class MemoKeywords(memo: Memo, keyWords: Set[KeyWord])

}
