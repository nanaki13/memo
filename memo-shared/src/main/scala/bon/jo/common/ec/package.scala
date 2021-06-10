package bon.jo.common
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
package ec :

    type TR[A,B] =  [A,B] => A ?=> B
    
    type Exec[A] = ExecutionContext ?=> Future[A]
