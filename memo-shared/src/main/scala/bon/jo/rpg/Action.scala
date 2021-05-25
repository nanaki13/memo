package bon.jo.rpg

import bon.jo.rpg.Action.ActionCtx.ActionCibled
import bon.jo.rpg.Action.Attaque.{MainDroite, MainGauche}
import bon.jo.rpg.Action.{ActionCtx, readCibleRec}
import bon.jo.rpg.StdinUtil.fromStdin
import BattleTimeLine._
import scala.concurrent.Future
import bon.jo.rpg.stat.GameId
import bon.jo.rpg.stat.GameElement


sealed trait Action extends Product:
  val name = toString

  def fromStdIn(cible: BattleTimeLine.LTP[GameElement]): ActionCtx =
    new ActionCibled(this, readCibleRec(cible))


object Action:


  given Action.Soin.type = Action.Soin
  given Action.Attaque.type = Action.Attaque
  given Action.Aoe.type = Action.Aoe
  given Action.Garde.type = Action.Garde
  given Action.Evasion.type = Action.Evasion
  given Action.Voler.type = Action.Voler
  given Action.ChangerDequipement.type = Action.ChangerDequipement
  given Action.Talent.type = Action.Talent
  given Action.Rien.type = Action.Rien
  given Action.Hate.type = Action.Hate
  given Action.Slow.type = Action.Slow
  given Action.Cancel.type = Action.Cancel


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
  trait ActionCtx:
    def action: Action

    def cible: Iterable[GameId.ID]

  object ActionCtx:

    object Garde extends ActionWithoutCible(Action.Garde)

    object Rien extends ActionWithoutCible(Action.Garde)

    case class ActionWithoutCible(val action: Action) extends ActionWithoutCibleOps

    class ActionCibled(val action: Action, val cible: Iterable[GameId.ID]) extends ActionCtx

  trait ActionWithoutCibleOps extends ActionCtx:
    override def cible = Nil


  def fromStdIn(d: TPA, cible: LTPA): Future[ActionCtx] =
    println(s"choisir action de ${d.simpleName}")
    Future.successful(fromStdIn match {
      case Attaque.MainGauche => Attaque.MainGauche.fromStdIn(cible)
      case Attaque.MainDroite => Attaque.MainDroite.fromStdIn(cible)
      case Garde => ActionCtx.Garde
      case Rien => ActionCtx.Rien
      case _ => ActionCtx.Rien
    })



  def fromStdIn: Action =
    fromStdin(Action.commonValues)


  def readCibleRec(cible: ITP[GameElement]):List[GameId.ID] =
    def f(t: TP[GameElement]) = t.simpleName
    List(fromStdin(cible, f,_.id))













