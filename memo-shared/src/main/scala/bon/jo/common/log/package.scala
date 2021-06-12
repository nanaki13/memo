package bon.jo.common
import bon.jo.common.ec.*
import scala.util.Failure
package  log :

    import scala.reflect.ClassTag
    import scala.concurrent.Future
    trait Logged[A](using ClassTag[A],Map[ClassTag[_],Boolean]):
        val logger = SimpleLog[A]
        inline def loga(s : Any) = logger.loga(s)
        inline def log(s : Any) =logger.log(s)
  
    trait SimpleLog[T](val name : String,val debug : Boolean):
        
        private[jo] inline def dolog(s : Any) = println(s"$name : ${TestMacro.inspect(s)}")
        inline def log(s : Any) = if debug then dolog(s) 
        inline def loga(s : Any) =    if debug then      
                val a : Exec[Unit] = Future(dolog(s))
                SimpleLog.run(a)
            
    object SimpleLog:
        class Impl[T](using ct : ClassTag[T],conf : Map[ClassTag[_],Boolean]) extends SimpleLog[T](ct.runtimeClass.getName,conf.getOrElse(ct,false))
        
        import scala.concurrent.ExecutionContext.Implicits.given
        def run(e : Exec[Unit]) = e.onComplete{
            case Failure(f) => f.printStackTrace
            case _ => 
        }
    
        val all = collection.mutable.Map[ClassTag[_],SimpleLog[_]]()
        def apply[T](using ct : ClassTag[T],conf : Map[ClassTag[_],Boolean] ) : SimpleLog[T]= 
            
            all.getOrElseUpdate(ct,Impl[T]()).asInstanceOf[SimpleLog[T]]


