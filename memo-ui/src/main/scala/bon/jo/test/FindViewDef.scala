package bon.jo.test

import bon.jo.html.HtmlEventDef.ExH
import bon.jo.memo.Entities.KeyWord
import bon.jo.test.HtmlRep.{HtmlCpnt, HtmlRepParam}
import org.scalajs.dom.raw.HTMLElement

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object FindViewDef {
  case class FindParam(
                        buttonName: String
                      )

  class FindView(val findParam: FindParam, val ctx: FindViewCtx) extends HtmlCpnt {
    implicit val ec = ctx.ec
    private val input = SimpleView.i
    private val button = SimpleView.bsButton(findParam.buttonName)

    override def get: Iterable[HTMLElement] = List(input, button)

    button.$click { _ =>
      ctx.memoTemplate.memosCOnr.clean()
      Daos.memoKeyWord.findByKeyWord(input.value).onComplete {
        case Failure(exception) => throw exception
        case Success(d) => d.foreach(ctx.memoTemplate.addMemo(_, ctx.kws))
      }
    }
    input.$Action {
      button.click()
    }
  }

  case class FindViewCtx(memoTemplate: MemoTemplate, kws: Iterable[KeyWord], ec: ExecutionContext)

  object FindViewProvider extends HtmlRepParam[FindParam, FindViewCtx] {
    override def html(memo: FindParam, p: Option[FindViewCtx]): HtmlCpnt = new FindView(memo, p.get)
  }

  implicit val pr: HtmlRepParam[FindParam, FindViewCtx] = FindViewProvider
}