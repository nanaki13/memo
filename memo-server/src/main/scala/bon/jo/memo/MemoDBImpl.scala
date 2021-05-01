package bon.jo.memo

import bon.jo.memo.Entities._
import slick.ast.{BaseTypedType, TypedType}
import slick.jdbc.JdbcType

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag


trait MemoDBImpl {
  self: DBProfile =>

  import self.profile.api._

  //implicit val memotTypreCv : TypedType[MemoType]

  class Memos(tag: Tag) extends Table[Memo](tag, "memo") {
 //   implicit val memoTypeCol: JdbcType[MemoType] with BaseTypedType[MemoType] = MappedColumnType.base[MemoType,String](_.toString,MemoType(_))
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)

    def content = column[String]("content",O.Length(1024*10))

    def title = column[String]("title")

    def memoType = column[String]("memo_type")


    def * = (id.?, title, content,memoType) <>[Memo] (MemoS.untupled_ ,MemoS.tupled_ )

    def idx = index("idx_content", content, unique = true)
  }

  class KeysWords(tag: Tag) extends Table[KeyWord](tag, "keys_word") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)

    def value = column[String]("value")

    def * = (id.?, value) <> (KeyWord.tupled, KeyWord.unapply)

    def idx = index("idx_value", value, unique = true)
  }


  class MemoKeywordsTable(tag: Tag) extends Table[MemoKeywordRel](tag, "memo_keys_word") {

    def idMemo = column[Int]("id_memo")

    def idKeyWord = column[Int]("id_key_word")

    def memoFk = foreignKey("memo_fk", idMemo, memos)(_.id, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)

    def keyWordFk = foreignKey("keyword_fk", idMemo, keyswords)(_.id, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)

    def * = (idMemo, idKeyWord) <> (MemoKeywordRel.tupled, MemoKeywordRel.unapply)

  }


  val memos = TableQuery[Memos]
  val keyswords = TableQuery[KeysWords]
  val memoKeywords = TableQuery[MemoKeywordsTable]

  val allinOrder = List(memos, keyswords, memoKeywords)

  def create(implicit db: DBProfile, executionContext: ExecutionContext): Future[List[Unit]] = Future.sequence(allinOrder.map(_.schema.create).map(db.db.run).map(_.recover(_ => ())))


}
