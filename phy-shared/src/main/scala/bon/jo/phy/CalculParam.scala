package bon.jo.phy

trait CalculParam extends Param {
  var dt = 0d

}

trait Param {
  //def forSun(doThat :  PointDynamic => Unit): Unit = sun foreach doThat

  var G :Double

  var kRessort: Double
  var speedFactor: Double

  var scaleTime: Double
  var frt: Double



 // var interaction: Interaction
  var correction: Boolean

}
object CalculParam {
  def apply(e: Param):CalculParam = {
    CalcuParamImpl(

      e.kRessort,
      e.speedFactor,
      e.scaleTime,
      e.frt,
     // e.interaction,
      e.correction,
      e.G
    )
  }
}