package bon.jo.rpg

import bon.jo.rpg.ActionResolver.Resolver
import bon.jo.rpg.stat._
import bon.jo.rpg.Action.Attaque
import scala.util.Random
import bon.jo.rpg.ui.PlayerUI
object CalculsPersoPerso extends  AttaqueResolve{

    type P = TimedTrait[GameElement]
    val r = Random()
    def resolve(attp:  TimedTrait[Perso],ciblep :P)(using ui : PlayerUI):P=
        ( attp.value , ciblep.value) match
            case (att : Perso,cible : Perso) =>
                val randomMagic  : Double = r.nextDouble * 0.15 + 0.85
                val randomPhy  : Double = r.nextDouble * 0.15 + 0.85
                val attM = att.stats.mag
                val attP = att.stats.str

                val deffM = cible.stats.psy
                val deffP = cible.stats.vit

                val degat = (((attM-deffM)/2 * randomMagic) +  (attP-deffP)/2 * randomPhy).toFloat.round
                uiProcess(ciblep.withValue(cible.copy(hpVar = cible.hpVar - degat)),degat)

    def uiProcess(perso : P,degat : Int)(using ui : PlayerUI):P=
        perso.value match
            case p : Perso =>
                ui.message(s"${p.name} a perdu ${degat} pv, il lui reste ${p.hpVar} pv",5000)
                ui.cpntMap(perso.id).update(Some(perso.cast))
                perso



}