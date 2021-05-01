package bon.jo.html

import bon.jo.phy.Obs
import org.scalajs.dom.raw.{Event, HTMLElement}

import scala.language.dynamics

case class EventAdder(el: HTMLElement, obs: Obs[Event] = Obs.once[Event]()) extends scala.Dynamic {


    def applyDynamic(method: String)(): this.type = {

      el.addEventListener(method, obs.newValue)
      this

    }
  }