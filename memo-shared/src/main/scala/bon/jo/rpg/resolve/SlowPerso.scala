package bon.jo.rpg.resolve

import bon.jo.rpg.AffectResolver.Resolver
import bon.jo.rpg.stat.*
import bon.jo.rpg.*
import scala.util.Random
import bon.jo.rpg.ui.PlayerUI
import bon.jo.rpg.resolve.PersoResolveContext.*
import bon.jo.rpg.BattleTimeLine.TimeLineParam
object SlowPerso extends  SlowResolve{

    type P = TimedTrait[GameElement]
    val r = Random()
    def resolveAffect(attp:  TimedTrait[Perso],ciblep :P) : PlayerUI.UI[P]=
        ( attp.value[Perso] , ciblep.value[GameElement]) match
            case (att : Perso,cible : Perso) =>
              

           

                val factor = calculFactor(attp,ciblep)
                val eff : FactorEffectt =  FactorEffectt(3,factor,Affect.Slow)
                uiProcess(ciblep.addEffect(eff),eff)

    def calculFactor(att : TimedTrait[Perso],defe : P)(using t : TimeLineParam):Float=
      /*  val attM = att.stats.mag
        
        val deffM = defe.stats.psy
             
        val randomMagic  : Float = r.nextFloat * 0.15f + 0.85f
        val max = 0.5f
        val min = 0.9f
        val fact = randomMagic  *( 2*attM-deffM)/(2*deffM )
        println(s"val $fact = $randomMagic  *( 2*$attM-$deffM)/(2*$deffM )")
        val delta = max - min
        fact * delta + min*/
        0.75
        


    def uiProcess(perso : P,factor : FactorEffectt)(using ui : PlayerUI):P=
        perso.value[Perso] match
            case p : Perso =>
                ui.message(s"${p.name} est ralenti de ${factor.factor} dans le temps pedans ${factor.time} tour",5000)
                ui.cpntMap(perso.id).update(Some(perso.cast))
                perso



}