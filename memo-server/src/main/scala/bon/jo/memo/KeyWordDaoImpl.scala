package bon.jo.memo



import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

class KeyWordDaoImpl(implicit val profile: DBProfile.DB) extends DaoImpl with Dao[Entities.KeyWord, Int] {

  import profile._
  import profile.profile.api._
  override def create(a: Entities.KeyWord): FO = {
    db.run(keyswords += a) flatMap {
      r =>
        if (r == 1) {
          db.run(keyswords.filter(_.value === a.value).sortBy(_.id.reverse).result.headOption)
        } else {
          Future.successful(None)
        }
    }
  }
  override def readAll(): FL =  db.run(keyswords.result)

   def findExact(query: Entities.KeyWord): FO = {
    db.run(keyswords.filter(_.value === query.value).result.headOption)
  }
   def findLike(query:  String): FL = {
    db.run(keyswords.filter(_.value.like(s"%$query%")).result)
  }
  override def update(a: Entities.KeyWord,id : Option[Int]): FO = {
    val e = id match {
      case Some(_) => a.copy(id)
      case None => a
    }
    db.run(keyswords.filter(_.id === e.id).update(e)).map {
      case 1 => Some(e)
      case 0 => None
      case _ => throw new IllegalStateException("plus d'une ligne a été mis a jour")
    }
  }

  override def read(a: Int): FO = db.run(keyswords.filter(_.id === a).result.headOption)

  override def delete(a: Int): FB = db.run(keyswords.filter(_.id === a).delete).map {
    case 1 => true
    case 0 => false
    case _ => throw new IllegalStateException("plus d'une ligne a été supprimer")
  }
}


