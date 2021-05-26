package bon.jo.rpg

import bon.jo.rpg.AffectResolver.Resolver
import bon.jo.rpg.stat._

import scala.util.Random
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.resolve.PersoResolveContext._
object CancelPerso extends  CancelResolve{

    type P = TimedTrait[GameElement]
    val r = Random()
    def resolveAffect(attp:  TimedTrait[Perso],ciblep :P)(using ui : PlayerUI):P=
        ( attp.value , ciblep.value) match
            case (att : Perso,cible : Perso) =>
                val randomMagic  : Double = r.nextDouble * 0.15 + 0.85

                val attM = att.stats.mag
                

                val deffM = cible.stats.psy
             

                val recul = (((attM-deffM) * randomMagic)).toFloat.round*5
                uiProcess(ciblep.withPos(ciblep.pos-recul),recul)

    def uiProcess(perso : P,recul : Int)(using ui : PlayerUI):P=
        perso.value match
            case p : Perso =>
                ui.message(s"${p.name} été reculé de ${recul} dans le temps",5000)
                ui.cpntMap(perso.id).update(Some(perso.cast))
                perso



}