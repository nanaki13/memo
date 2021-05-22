package bon.jo.rpg.dao

import bon.jo.app.Export.PersoJS
import bon.jo.dao.LocalJsDao
import bon.jo.dao.LocalJsDao.{MappedDao, MappedDaoImpl}
import bon.jo.rpg.stat.raw.Perso
import bon.jo.util.Mapper

import scala.concurrent.ExecutionContext

trait PersoDao:
  self: MappedDao[PersoJS, Perso] =>

object PersoDao {

  trait PersoDaoJs extends LocalJsDao[PersoJS]:
    val name = "PersoDao"
    val fId: PersoJS => Int = _.id


  implicit object PersoMapper extends Mapper[Perso, PersoJS]:
    override val map: Perso => PersoJS = PersoJS.apply
    override val unmap: PersoJS => Option[Perso] = PersoJS.unapply

  def apply(jsDao: PersoDaoJs)(implicit executionContext: ExecutionContext): MappedDao[PersoJS, Perso] with PersoDao =
    new MappedDaoImpl(jsDao) with PersoDao
}