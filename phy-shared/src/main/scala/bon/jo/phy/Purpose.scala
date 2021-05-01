package bon.jo.phy

object Purpose {




 // case object PlanetTarget extends Purpose
 case object  Find extends Named("Trouver") with Purpose
  case object Move extends Named("Déplacer") with Purpose
  case object Delete extends Named("Suppromer") with Purpose
  case object DontFollow extends Named("Ne plus suivre") with Purpose
  case object Follow extends Named("Suivre") with Purpose
  case object Create extends Named("Créer") with Purpose
  case object Void extends Named("Rien") with Purpose with What
  case object All extends Named("Tout") with Purpose with What
  def apply(puport : String): Purpose = all.find(_.toString == puport).get

  val all = List(Move,Delete,Create,Void,DontFollow,Follow)
  object What{


    case object Point extends What
    case object Interaction extends What{
      object Type{
        def apply(s : String): Type = all(s)
      }
      sealed trait Type{
        def name : String
        def  factor : Double
        def unary_! : Type = {
          this match {
            case Attractive => Repulsive
            case Repulsive => Attractive
          }
        }
      }
      case object Attractive extends NamedWithFactor("Attractive",1D)  with Type
      case object Repulsive extends NamedWithFactor("Repulsive",-1D)  with Type
      val all: Map[String,Type] = List[Type](Attractive,Repulsive).map {e => (e.name,e)}.toMap
    }

  }
  sealed trait  What
  abstract class NamedWithFactor(name : String,val factor : Double) extends  Named(name)
  abstract class Named(val name : String)
}

sealed  trait Purpose extends NamedOps
trait NamedOps{
  def name : String
}