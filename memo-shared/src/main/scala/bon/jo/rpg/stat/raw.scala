package bon.jo.rpg.stat

import bon.jo.rpg.stat

object raw {
  type Actor = stat.Actor
  val Actor: stat.Actor.type = stat.Actor
  type Alg[A] = stat.Alg[A]
  val Alg: stat.Alg.type = stat.Alg
  type AnyRefBaseStat[+A] = stat.AnyRefBaseStat[A]
  type FloatBaseStat = stat.AnyRefBaseStat[Float]
  type IntBaseStat = stat.AnyRefBaseStat[Int]
  type StringBaseStat = stat.AnyRefBaseStat[String]
  val AnyRefBaseStat: stat.AnyRefBaseStat.type = stat.AnyRefBaseStat
  type ArmedActor = stat.ArmedActor
  val BaseState: stat.BaseState.type = stat.BaseState
  type Perso = stat.Perso
  val Perso: stat.Perso.type = stat.Perso
}