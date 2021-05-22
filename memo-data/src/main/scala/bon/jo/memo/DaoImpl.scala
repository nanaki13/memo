package bon.jo.memo

import slick.lifted.{AbstractTable, Query, TableQuery}

trait DaoImpl {
  implicit val profile: DBProfile
  val db: profile.profile.backend.DatabaseDef = profile.db

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
