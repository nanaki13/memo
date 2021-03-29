package bon.jo.memo

import bon.jo.memo.Entities._
import slick.ast.{BaseTypedType, TypedType}
import slick.jdbc.JdbcType

import scala.concurrent.{ExecutionContext, Future}


trait MemoDBImpl {
  self: DBProfile =>

  import self.profile.api._

  //implicit val memotTypreCv : TypedType[MemoType]
  implicit val memoTypeCol: JdbcType[MemoType] with BaseTypedType[MemoType] = MappedColumnType.base[MemoType,String](_.toString,MemoType(_))
  class Memos(tag: Tag) extends Table[Memo](tag, "MEMO") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def content = column[String]("CONTENT",O.Length(1024*10))

    def title = column[String]("TITLE")

    def memoType = column[MemoType]("MEMO_TYPE")

    def * = (id.?, title, content,memoType) <> (Memo.tupled, Memo.unapply)

    def idx = index("idx_content", content, unique = true)
  }

  class KeysWords(tag: Tag) extends Table[KeyWord](tag, "KEYS_WORD") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)

    def value = column[String]("VALUE")

    def * = (id.?, value) <> (KeyWord.tupled, KeyWord.unapply)

    def idx = index("idx_value", value, unique = true)
  }


  class MemoKeywordsTable(tag: Tag) extends Table[MemoKeywordRel](tag, "MEMO_KEYS_WORD") {

    def idMemo = column[Int]("ID_MEMO")

    def idKeyWord = column[Int]("ID_KEY_WORD")

    def memoFk = foreignKey("MEMO_FK", idMemo, memos)(_.id, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)

    def keyWordFk = foreignKey("KEYWORD_FK", idMemo, keyswords)(_.id, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)

    def * = (idMemo, idKeyWord) <> (MemoKeywordRel.tupled, MemoKeywordRel.unapply)

  }


  val memos = TableQuery[Memos]
  val keyswords = TableQuery[KeysWords]
  val memoKeywords = TableQuery[MemoKeywordsTable]

  val allinOrder = List(memos, keyswords, memoKeywords)

  def create(implicit db: DBProfile, executionContext: ExecutionContext) = Future.sequence(allinOrder.map(_.schema.createIfNotExists).map(db.db.run))


}
