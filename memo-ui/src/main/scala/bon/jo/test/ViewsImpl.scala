package bon.jo.test

import bon.jo.html.HtmlEventDef._
import bon.jo.memo.Entities
import bon.jo.memo.Entities.KeyWord
import bon.jo.test.HTMLDef._
import bon.jo.test.HtmlRep.PrXmlId
import bon.jo.test.SimpleView.i
import org.scalajs.dom.html.{Div, Input}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}






class ViewsImpl(implicit executionContext: ExecutionContext) {


  val viewsDef: ViewsDef = ViewsDef()

  import viewsDef.KewWordHtml.WithClose._

   val keywWordI: Input = i
  val kewWordDiv: Div = $c.div[Div]
  object keyWordView extends SimpleView[Entities.KeyWord](() => $va div ($va span($t("titre :")
    , keywWordI)), (kw)=> kewWordDiv ++= kw.html.list )

  def addKwEvent: Unit =
    keyWordView.btnInput.$click{ _=>
      val m = Entities.KeyWord(None, keywWordI.value)
      val req: Future[Unit] = Daos.keyWordDao.create(m).map(o => o.foreach(keyWordView.+=))
      req.onComplete {
        case Failure(exception) => throw (exception)
        case Success(_) =>
      }

    }


}


