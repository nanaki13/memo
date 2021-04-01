package bon.jo.memo

import scala.concurrent.{Await, ExecutionContext, Future, duration}

object test2 extends App {
  type DB = DBProfile with MemoDBImpl
  implicit val db : DB= DBProfile.value

  val dao = new KeyWordDaoImpl()
  def run(implicit executionContext: ExecutionContext): Unit ={
    db.create

    val fut = dao create Entities.KeyWord(None, "toto") flatMap {
      case None => Future.failed(new IllegalStateException())
      case Some(value) =>
        println(value)
        dao findLike "t" map {
          case Nil => println("rien")
          case a => println(a)
        }
    }
    Await.result(fut, duration.Duration.Inf)
  }



}
