package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx
import bon.jo.rpg.Action.PlayerUIStdIn.value
import bon.jo.rpg.BattleTimeLine.TimeLineParam
import bon.jo.rpg.DoActionTrait.WithAction

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global














object TestRPG extends App {
  var id = 0

  def getid() = {
    id += 1
    id
  }





  //  trait ActionOps[A] {
  //    def resolveAction[B](a : A, action: Action, B : B):Unit
  //  }









  case class Perso(name: String, att: Int, var speed: Int, var life: Int, var action: ActionCtx, id: Int = getid()) {
    // override def toString: String = name
  }

  case class Mur()

  implicit object PersoEx extends PersoOps

  implicit object PeroPero extends Timed[Perso] {

    val posCache = mutable.Map[Int, Int]()


    override def speed(a: Perso): Int = a.speed

    override def action_=(a: Perso, action: ActionCtx): Unit = a.action = action

    override def action(a: Perso): ActionCtx = a.action

    override def pos(a: Perso): Int = {
      posCache.getOrElse(a.id, 0)
    }

    override def pos_=(a: Perso, pos: Int): Unit = posCache(a.id) = pos

    override def simpleName(value: Perso): String = value.name
  }


  trait PersoOps extends Actions[Perso, List[TimedTrait[_]]] {
    def resolve(a: Perso, action: Action, b: List[TimedTrait[_]]): Unit = {
      action match {
        case Action.AttaqueMainGauche | Action.AttaqueMainDroite =>
          b.map(_.value) match {
            case List(p: Perso) =>
              p.life -= a.att
              println(s"${p.name} a perdu ${a.att} pv, il lui reste ${p.life} pv")
          }
        case Action.Defendre =>
        case Action.Rien =>
      }


    }
  }

  val p1 = Perso("Bob", 1, 2, 3, ActionCtx.Rien)
  val p2 = Perso("Bill", 1, 2, 3, ActionCtx.Rien)
  val m = Mur()


  val yl = TimeLineParam(0, 50, 70)
  yl.add(p1)
  yl.add(p2)
  implicit val acImpl: Actions[TimedTrait[Any], List[TimedTrait[_]]] = {
    (a: TimedTrait[_], action: Action, b: List[TimedTrait[_]]) =>
      a.value match {
        case e: Perso => e.resolve(action, b.value)
      }
  }

  for (_ <- 1 to 100) {
    yl.nextState
  }



}










