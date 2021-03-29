package bon.jo.memo

import slick.lifted.AbstractTable

trait DaoImpl {
  implicit val profile: DBProfile
  val db: profile.profile.backend.DatabaseDef = profile.db

  import profile.profile.api._

  def applyLimit[A <: AbstractTable[_]](tableQuery: TableQuery[A], limit: Int, offset: Int): Query[A, A#TableElementType, Seq] = {

    val q1 = if (offset != -1) {
      tableQuery.drop(offset)
    } else {
      tableQuery
    }
    if (limit != -1) {
      q1.take(limit)
    } else {
      q1
    }

  }
}
