package bon.jo.memo

import bon.jo.dao.Dao
import bon.jo.dao.Dao.FB

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{ExecutionContext, Future}



class MemoDaoImpl(implicit val profile: DBProfile.DB) extends  DaoImpl with Dao[Entities.Memo, Int] {
  import profile._
  import profile.profile.api._
  override def create(a: Entities.Memo): FO = {
    db.run(memos += a) flatMap {
      r =>
        if (r == 1) {
          db.run(memos.filter(_.content === a.content).sortBy(_.id.reverse).result.headOption)
        } else {
          Future.successful(None)
        }
    }
  }

  override def update(a: Entities.Memo,id : Option[Int]): FO = {
    db.run(memos.filter(_.id === a.id).update(a)).map {
      case 1 => Some(a)
      case 0 => None
      case _ => throw new IllegalStateException("plus d'une ligne a été mis a jour")
    }
  }

  override def read(a: Int): FO = db.run(memos.filter(_.id === a).result.headOption)

  override def delete(a: Int): FB = db.run(memos.filter(_.id === a).delete).map {
    case 1 => true
    case 0 => false
    case _ => throw new IllegalStateException("plus d'une ligne a été supprimer")
  }



   def findLike(query:  String): FL = {
    db.run(memos.filter(_.content.like(query)).result)
  }

   def findExact(query: Entities.Memo): FO = {
    db.run(memos.filter(_.content === query.content).result.headOption)
  }

  override def readAll(limit : Int,offset : Int): FL =  db.run(applyLimit(memos,limit,offset).result)

  override def readAll(a: Iterable[Int])(implicit executionContext: ExecutionContext): FL = db.run(memos.filter(_.id inSet  a).result)
}


