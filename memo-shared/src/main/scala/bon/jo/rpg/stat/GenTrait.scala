package bon.jo.rpg.stat

import bon.jo.rpg.stat.Actor.Implicit._
import bon.jo.rpg.stat.Actor.{randomActor, randomWeapon}
object GenTrait {

  case class DocAndName(name: String, doc: String) {
    def this(a: Array[String]) = {
      this(a(0), a(1))
    }

    def definition =
      s"""
         | /**
         | * ${doc}
         | * @return value
         | **/
         |def `$name`: Any""".stripMargin
  }

  val str =
    """Bonus Vie: Ajout de points de vie
      |Bonus SP: Ajout de point de Compétence
      |Bonus Vivacité ou Bonus VIV: Ajout de points de VIV
      |Puissance physique ou Pwr : points utilisés dans le calcul des dégâts physiques d’une attaque
      |Pwr-M : points utilisés dans le calcul des dégâts magiques d’une attaque
      |Bonus RES : point utilisés dans la résolution des affects néfastes
      |Armure phy ou Arm-phy: utilisé dans le calcul de la défense physique
      |Armure mag ou Arm-M: utilisé dans le calcul de la défense magique
      |Critical rate ou Crt: influence les chances de critique
      |""".stripMargin

  def docAndName = str.split("\n").map(e => new DocAndName(e.split(":")))


}
