package bon.jo.rpg
import bon.jo.rpg.resolve.PersoResolveContext._

package resolve {
    given  ResolveContext = new ResolveContext{
       def attaqueResolve = CalculsPersoPerso
       def soinResolve = SoinPerso
       def cancelResolve = CancelPerso
      // def gardeResolve = ResolveContext.unknwon[A.Garde.type]()

  }
}
