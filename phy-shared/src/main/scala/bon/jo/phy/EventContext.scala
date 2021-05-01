package bon.jo.phy

import bon.jo.phy.EventContext.obs
import bon.jo.phy.Phy.P
import bon.jo.phy.view.{UIParams, ViewPort}


class Selection[A <: WithId](val selected: Option[A]){
  def id: Int = selected.get.id
}


object NoneSelection extends Selection[Nothing](None)

//object PlaneteSelection {
//  def apply[A <: PointDynamic](selected: A): PlaneteSelection[A] = PlaneteSelection(Some(selected))
//}

abstract class PlaneteSelection[A <: PointDynamic with WithId](override val selected: Option[A]) extends Selection[A](selected) {
  def cp(): PlaneteSelection[A]


}

abstract class InteractionSelection[P <: PointDynamic with WithId, A <: PointInteraction[P]](override val selected: Option[A]) extends Selection[A](selected) {
  def cp(): InteractionSelection[P,A]
}

//object  InteractionSelection{
//  def apply[A <: PointDynamic](selected: PointInteraction[A]): InteractionSelection[A] = InteractionSelection(Some(selected))
//}
sealed trait Source

object Source {

  case object UI extends Source

  case object Ctrl extends Source

}

case class EmittedValue[A](value: A, source: Source)

case class EventContext[ExpType, ImptType](
                                            scaleTime: Obs[Double] = obs(),
                                            newElemtsMasse: Obs[Double] = obs(),
                                            tracer: Obs[Boolean] = obs(),
                                            sizeFactor: Obs[Double] = obs(),
                                            soleilMasse: Obs[Double] = obs(),
                                            goTo: Obs[P] = obs(),
                                            speedFactor: Obs[Double] = obs(),
                                            pushPull: Obs[Boolean] = obs(),
                                            clean: Obs[Unit] = obs(),
                                            ineraction: Obs[bon.jo.phy.Interaction] = obs(),
                                            frotement: Obs[Double] = obs(),
                                            correction: Obs[Boolean] = obs(),
                                            replaceAround: Obs[Unit] = obs(),
                                            stabilise: Obs[Boolean] = obs(),
                                            action: Obs[ActionPointDynamic[_, _]] = obs(),
                                            //  actionPoint: Obs[ActionPoint] = obs(),
                                            viewPort: Obs[EmittedValue[ViewPort]] = obs(),
                                            uiParams: Obs[EmittedValue[UIParams]] = obs(),
                                            selectionCtrlToUi: Obs[Selection[_]] = obs(),
                                            selectionUpdateUiToCtrl: Obs[Selection[_]] = obs(),
                                            opeationOnElementDone: Obs[(Purpose, Purpose.What, Selection[_])] = obs(),
                                            userChoice: Obs[(Purpose.What, Selection[_])] = obs(),
                                            userWant: Obs[Purpose] = obs(),
                                            saveModel: Obs[Unit] = obs(),
                                            modelForSave: Obs[ExpType] = obs[ExpType](),
                                            modelImport: Obs[ImptType] = obs[ImptType](),
                                            resetModelView : Obs[Unit] = obs()
                                            //   uIParams: Obs[EmittedValue[UIParams]]
                                          ) {



}

//UI
object EventContext {
  def obs[T](): Obs[T] = Obs.once[T]().toMany

  implicit def obsFact[A]: ObsFact[A] = () => EventContext.obs[A]()
}