package bon.jo.phy

import bon.jo.phy.Purpose.What

case class Model[A <: PointDynamic with WithId](var points: List[A], var interactions : List[PointInteraction[A]])

case class PointInteraction[A <: PointDynamic with WithId](p : A,var interaction: Interaction,var _type : What.Interaction.Type = What.Interaction.Attractive) extends WithId{
  override def id: Int = p.id
}
