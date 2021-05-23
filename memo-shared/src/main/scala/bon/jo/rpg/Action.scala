package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx.ActionCibled
import bon.jo.rpg.Action.Attaque.{MainDroite, MainGauche}
import bon.jo.rpg.Action.{ActionCtx, readCibleRec}
import bon.jo.rpg.StdinUtil.fromStdin
import BattleTimeLine.{TP,LTP,ITP}
import scala.concurrent.Future


sealed trait Action extends Product:
  val name = toString

  def fromStdIn[A](cible: BattleTimeLine.LTP[A]): ActionCtx[A] =
    new ActionCibled(this, readCibleRec(cible))


object Action:



  case object Attaque extends Action:
    case object MainDroite extends Action

    case object MainGauche extends Action

  case object Soin extends Action
  case object Aoe extends Action
  case object Garde extends Action
  case object Evasion extends Action
  case object Voler extends Action
  case object ChangerDequipement extends Action
  case object Talent extends Action
  case object Rien extends Action
  case object Hate extends Action
  case object Slow extends Action
  case object Cancel extends Action
  val all = List(Attaque,
    MainGauche,
    MainDroite,
    Soin,Aoe,Garde,Evasion,Voler,ChangerDequipement,Talent,Rien,Hate,Slow,Cancel)
  def unapply(string: String): Option[Action] = applyFrom(all.toSet)(string)
  def applyFrom(from : Set[Action])(string: String) : Option[Action] =

    from.map(e=> {

      e
    }).find(_.name == string)
  val commonValues: List[Action] = List(Voler , Garde,Evasion, Rien, ChangerDequipement)
  val weaponValues: Iterable[Action] = List(Attaque,Soin,Hate,Slow,Cancel)
  trait ActionCtx[A]:
    def action: Action

    def cible: BattleTimeLine.LTP[A]

  object ActionCtx:

    def Garde[A] :  ActionWithoutCible[A] = ActionWithoutCible(Action.Garde)

    def Rien[A] :  ActionWithoutCible[A] = ActionWithoutCible(Action.Garde)

    case class ActionWithoutCible[A](val action: Action) extends ActionWithoutCibleOps[A]

    class ActionCibled[A](val action: Action, val cible: BattleTimeLine.LTP[A]) extends ActionCtx[A]

  trait ActionWithoutCibleOps[A] extends ActionCtx[A]:
    override def cible: BattleTimeLine.LTP[A] = Nil


  def fromStdIn[A](d: TP[A], cible: LTP[A]): Future[ActionCtx[A]] =
    println(s"choisir action de ${d.simpleName}")
    Future.successful(fromStdIn match {
      case Attaque.MainGauche => Attaque.MainGauche.fromStdIn(cible)
      case Attaque.MainDroite => Attaque.MainDroite.fromStdIn(cible)
      case Garde => ActionCtx.Garde[A]
      case Rien => ActionCtx.Rien[A]
      case _ => ActionCtx.Rien[A]
    })



  def fromStdIn: Action =
    fromStdin(Action.commonValues)


  def readCibleRec[A](cible: LTP[A]): LTP[A] =
    def f(t: TP[A]) = t.simpleName
    List(fromStdin(cible, f))













