package bon.jo.memo



import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{ExecutionContext, Future}

class KeyWordDaoImpl(implicit val profile: DBProfile.DB) extends DaoImpl with Dao[Entities.KeyWord, Int] {

  import profile._
  import profile.profile.api._
  override def create(a: Entities.KeyWord): FO = {

    db.run(keyswords += a).recoverWith{
      case e =>
        e.printStackTrace()
        db.run(keyswords += a.copy(id = Some(1)))
    } flatMap  {
      r =>
        if (r == 1) {
          db.run(keyswords.filter(_.value === a.value).sortBy(_.id.reverse).result.headOption)
        } else {
          Future.successful(None)
        }
    } recover{
      e =>
        e.printStackTrace()
        None
    }
  }

  def findId(query : String):FIL = db.run(keyswords.filter(_.value.like(s"%$query%")).map(_.id).result)

  override def readAll(limit : Int,offset : Int): FL =  {

    db.run(applyLimit(keyswords,limit,offset).result)

  }

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

  override def readAll(a: Iterable[Int])(implicit executionContext: ExecutionContext): FL = db.run(keyswords.filter(_.id inSet  a).result)
}


