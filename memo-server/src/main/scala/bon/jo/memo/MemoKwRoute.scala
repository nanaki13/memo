package bon.jo.memo

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import bon.jo.dao.Dao
import bon.jo.memo.Entities.MemoKeywords
import bon.jo.memo.RestRoutes.{CustomRoute, RestRoutesImpl}
import org.json4s.Formats

class MemoKwRoute(name: String)(
   implicit _dao : Dao[MemoKeywords, Int],
   _formats: Formats,
   _materializer: Materializer,
   _manifest: Manifest[MemoKeywords]
)  extends RestRoutesImpl[MemoKeywords](name) with CustomRoute with ReqResConv[MemoKeywords]{
  override def custom: Route = {
    get{
      path(name / "find"){
         parameters("query".?){
           case Some(v) => dao match {
             case a:  Dao.StringQuery =>
               resolve(a.find(v))
           }
           case None => resolve(dao.readAll())
         }
      }
    }
  }
}

