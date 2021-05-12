package bon.jo.app

import bon.jo.rpg.stat.raw.Weapon

import scala.scalajs.js

object Export {
    object WeaponJS{
      def apply(weapon: Weapon)={
        js.Dynamic.literal(
          name = weapon.name,
          name = weapon.lvl,
          name = weapon.hp,
          name = weapon.name,
          name = weapon.name,
          name = weapon.name,
          name = weapon.name
        )
      }
    }
    trait WeaponJS extends js.Object{
      val name : String
      val id : Int
      val lvl : Int
      val hp: Int

      /**
       * permet d’utiliser les talents
       *
       * @return
       */
      val sp: Int

      /**
       * Gouverne la vitesse à laquelle le personnage se déplace sur la Timeline de combat.
       *
       * @return
       */
      val viv: Int

      /**
       * Ces points s'ajoutent au calcul des attaques physiques
       *
       * @return
       */
      val str: Int

      /**
       * Ces points s’ajoutent au calcul des attaques magiques
       *
       * @return
       */
      val mag: Int

      /**
       * Ces points servent au calcul de la défense physique
       *
       * @return
       */
      val vit: Int

      /**
       * Ces points servent au calcul de la défense magique
       *
       * @return
       */
      val psy: Int
    }
}
