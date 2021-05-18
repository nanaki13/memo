package bon.jo.app

import bon.jo.memo.BaseRoute
import bon.jo.memo.ui.Routing.{PFromStr, Path}

object Paths  {
  val appPath: Path = "app".p
  val pMemo: Path = appPath / BaseRoute.memoRoute
  val pFind: Path = pMemo / BaseRoute.find
  val pCreationKW: Path = appPath / BaseRoute.keywordRoute
}
