package bon.jo.util

import scala.concurrent.ExecutionContext

trait Ec:
  implicit val executionContext: ExecutionContext
