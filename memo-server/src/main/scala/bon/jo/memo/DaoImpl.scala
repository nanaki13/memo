package bon.jo.memo

trait DaoImpl {
  implicit val profile: DBProfile
  val db = profile.db
}
