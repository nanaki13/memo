package bon.jo.memo

import com.typesafe.config.ConfigFactory
import slick.jdbc.JdbcProfile

object DBProfile {
  type DB =  DBProfile with MemoDBImpl
   val value: DB = ConfigFactory.load().getString("memodb.driver") match {
    case "org.sqlite.JDBC" => Impl(slick.jdbc.SQLiteProfile)

    case _ => ???
  }
  case class Impl( profile: JdbcProfile) extends DBProfile with MemoDBImpl
}

trait DBProfile {
  val profile: JdbcProfile

  val dbFactory: profile.backend.DatabaseFactoryDef = profile.api.Database
  val db: profile.backend.DatabaseDef = dbFactory.forConfig("memodb")
}