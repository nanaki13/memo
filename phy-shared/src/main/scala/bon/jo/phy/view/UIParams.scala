package bon.jo.phy.view

import bon.jo.phy.Phy.{P, V}
import bon.jo.phy.Purpose.What
import bon.jo.phy.{Interaction, Param, view}

case class UIParams(


                     var newElemtsMasse: Double = 4000D,
                     var frt: Double = 0D,
                     var creationForceOppose : What.Interaction.Type = What.Interaction.Attractive,
                     var kRessort: Double = 1D,
                     var speedFactor: Double = 1d,
                     var tracerString: String = "non",
                     var tracer: Boolean = false,
                     var scaleTime: Double = 1d,
                     var sizeFactor: Double = 1d,
                     var maskColor: String = "#000F56",

                     var correction: Boolean = true,
                     var viewPort: ViewPort = view.ViewPort(1d, P(0), V(2400), V(0, 1200)),
                     var planeteChoice : List[Int] = Nil,
                     var G: Double  = 6.67E-1
                   )  extends Param {




}
