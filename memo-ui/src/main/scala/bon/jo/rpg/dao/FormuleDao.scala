package bon.jo.rpg.dao

import bon.jo.app.Export.PersoJS
import bon.jo.dao.LocalJsDao
import bon.jo.dao.LocalJsDao.{MappedDao, MappedDaoImpl}
import bon.jo.rpg.stat.raw.Perso
import bon.jo.util.Mapper

import scala.concurrent.ExecutionContext
import bon.jo.rpg.Affect
import bon.jo.dao.Dao

trait FormuleDao extends Dao[Formule,String]
trait FormuleJs extends scalajs.js.Object:
  val id : String
  val formule : String
case class Formule(affect : Affect,formule : String)
object FormuleDao {

  trait FormuleDaoJs extends LocalJsDao[FormuleJs,String]:
    val name = "FormuleDao"
    val fId: FormuleJs => String = _.id


  implicit object FormuleMapper extends Mapper[Formule, FormuleJs]:
    override val map: Formule => FormuleJs = f => (scalajs.js.Dynamic.literal( id = f.affect.name,formule = f.formule ).asInstanceOf[FormuleJs])
    override val unmap: FormuleJs => Option[Formule] = f => Some(Formule(affect = Affect.valueOf(f.id),formule = f.formule))

  def apply(jsDao: FormuleDaoJs)(implicit executionContext: ExecutionContext): MappedDao[FormuleJs, Formule,String] with FormuleDao =
    new MappedDaoImpl(jsDao) with FormuleDao
}