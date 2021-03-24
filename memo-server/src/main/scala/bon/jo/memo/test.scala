package bon.jo.memo

import scala.concurrent.{Await, ExecutionContext, Future, duration}

object test extends App {
  implicit val db: DBProfile.DB = DBProfile.value

  def run(implicit executionContext: ExecutionContext): Unit ={

    val dao = new MemoDaoImpl()

    db.create

    val fut = dao create Entities.Memo(None, "toto", "toto") flatMap {
      case None => Future.failed(new IllegalStateException())
      case Some(value) => dao create Entities.Memo(None, "toto", "toto") map {
        case None =>
        case Some(value) => println(value)
      }
    }

    Await.result(fut, duration.Duration.Inf)
  }


}
