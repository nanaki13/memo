package bon.jo.memo

object TestDao extends App {
  implicit val matcher: Dao.Id[String] = e => e

  import scala.concurrent.ExecutionContext.Implicits._

  val daoString = new Dao.ListDao[String, String]() {

  }
  daoString + "s1"
  (daoString --> "s1") foreach (println)
  (daoString - "s1")
  daoString --> "s1" foreach (println)
}
