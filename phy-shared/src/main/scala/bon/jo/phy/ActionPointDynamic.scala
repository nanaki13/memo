package bon.jo.phy

import bon.jo.phy.view.Shape
import bon.jo.phy.PointDynamic

class ActionPointDynamic[ P <: PointDynamic, PA](val p: P, val purpose: Purpose, val what: Purpose.What, val parameters: Option[PA] = None)

case class ActionPointDynamicNoParam[ P <: PointDynamic](override val p: P, override val purpose: Purpose, override val what: Purpose.What)
  extends ActionPointDynamic[ P, Nothing](p: P, purpose, what, None)

case class ActionPointDynamicParam[ P <: PointDynamic, PA](override val p: P, override val purpose: Purpose, override val what: Purpose.What, override val parameters: Option[PA])
  extends ActionPointDynamic[ P, PA](p: P, purpose, what, parameters)