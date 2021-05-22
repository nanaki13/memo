package bon.jo.memo
import bon.jo.dao.Dao
import scala.concurrent.ExecutionContext.Implicits._
object TestDao extends App {
  implicit val matcher: Dao.Id[String] = e => e

  val daoString = new Dao.ListDao[String, String]() {

  }
  daoString + "s1"
  (daoString --> "s1") foreach (println)
  (daoString - "s1")
  daoString --> "s1" foreach (println)
}
