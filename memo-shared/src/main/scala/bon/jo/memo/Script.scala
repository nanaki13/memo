package bon.jo.memo

import bon.jo.rpg.stat.Perso
import scala.compiletime.ops.string
import bon.jo.rpg.stat.Actor
import bon.jo.rpg.stat.BaseState
import scala.util.Random
import Script.Node._
import scala.collection.mutable.ListBuffer
import bon.jo.memo.Script.PhraseElement
import Script.*
import scala.reflect.ClassTag
import bon.jo.common.ec.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

package give:
    given OpenCLose = (Exper.`(`,Exper.`)`)


object Script:

    val logger = SimpleLog[Script.type]
    val preDef :Map[String,(()=>Float)] = Map("rand" -> Random.nextFloat)

    given [A<:Product,B<:Product](using prefix : List[String]) :  ToFunction[String,(A,B)]  = 
            
        s =>
        preDef.map{
            e =>
                (e._1,(t :( A,  B)) => e._2.apply())}.getOrElse(s,{
                    val (pref,index) =  prefix.zipWithIndex.find(_._1 == s.substring(0,s.indexOf("."))).getOrElse(throw IllegalStateException(s"no prefix $s configured"))
                    val dottedp = dotted(pref)
                        
                    index match
                        case 0 => 
                            val f = stringFunction[A](s.replaceFirst(dottedp,""))
                            (p: A,_: B)=> f(p)
                        case 1 => 
                            val f = stringFunction[B](s.replaceFirst(dottedp,""))
                            (_ : A,p : B)=> f(p)
                }

                )
        

    given [A<:Product](using prefix : String) :  ToFunction[String,A]  = 
        val dottedp = dotted(prefix) 
        val f = stringFunction[A]
        s =>
        if prefix != s.substring(0,s.indexOf(".")) then throw IllegalStateException(s"no prefix $s configured")
        else 
            p => f(s.replaceFirst(dottedp,""))(p)  
    

    enum Node[+A]:
        case Root( childs : List[Node[A]])
       // case Regular( childs : List[Child[A]])
        case Value(val values : List[A]) 

        def groupValue[B](f :Value[A]=>B,combine : (B,B)=>B): B=
          
            this match
                case a : Value[A] => f.apply(a)
                case Root(childs) => 
                   
                    val re = childs.map{
                    case  z : Value[A] => f.apply(z)
                    case  r => r.groupValue(f,combine)
                    }.reduceLeft(combine)
                    
                    re
        def parentOption : Option[Root[A]] = 
            this match 
                case e :  Root[A] => Some(e)
                case _ => None

    
    def expressionPure( values : Value[PhraseElement])(minorPriority : Seq[PhraseElement.Symbol] = Seq.empty):Exper =
        values.values.foldLeft(Exper.Empty)(_.conbineRight(_)(minorPriority.map(_.str) : _ * ))
    
    def expressionPureWithAsso( values : Value[PhraseElement])(minorPriority : PhraseElement.Symbol *):Exper =
        var ret : Exper = Exper.Empty
        val buff : List[PhraseElement] =  values.values

        val fist = minorPriority.map(buff.indexOf(_)).find(_ != -1).getOrElse(-1) 
        val haveOther = buff.slice(fist,buff.size).map(buff.indexOf(_)).find(_ != -1).getOrElse(-1) 
      
       
        if(fist != -1 && haveOther  != -1) then
            val (head,tail) = buff.splitAt(fist+1)
            expressionPure(Value(head))(minorPriority).conbineRight(expressionPureWithAsso(Value(tail))(minorPriority : _ *)) 

        else
           expressionPure(values)(minorPriority)   

       
    extension (a : Node[PhraseElement])
        def toExpression() : Exper = 
            a.groupValue(v => Script.expressionPure(v)() ,_ conbineRight _)
        def toExpressionWithAsso() : Exper = 
            a.groupValue(Script.expressionPureWithAsso(_)(Exper.`+`,Exper.`-`) ,_ conbineRight _)
        
    object Node:
        
        case class Ctx[A](values : List[A],result: Root[A],parent : Ctx[A] ):
            def close : Ctx[A] = 
                val me = flush
                parent.copy(result = parent.result.copy(childs = parent.result.childs :+ me.result))

            def flush : Ctx[A]=
                if !values.isEmpty  then
                    val v = Value[A](values.toList)    
                    copy(values = Nil,result = result.copy(result.childs :+ v))
                else
                    this

        def apply[A](list : Iterable[A])(using openClose : (A,A)):Node.Root[A]=
            val cxt:  Ctx[A] = Ctx(Nil,Root[A](Nil),null)       
            val (open,close) = openClose
            list.foldLeft(cxt){
                (n,v) =>
                  
                    if v==open then
                       Ctx(Nil,Root[A](Nil),n.flush)             
                    else 
                    if v == close then  
                       n.close  
                    else 
                       n.copy(values = n.values :+ v)
            }.flush.result

    extension [A <: Product] (p : A)
        def nameToProp : Iterator[(String,Any)] = p.productElementNames zip p.productIterator
    trait ToFunction[V,T]:
        def apply(v:V) :  T  => Float
    object ToFunction:
        def apply[V,T](f :( V => T  => Float) ) : ToFunction[V,T] = v => t => f(v)(t)
     
    def dotted(s : String) = s"${s}."  
    def stringFunction[A <: Product] : String => A => Float = s => a => a.nameToProp.find(_._1 == s).map(_._2).get.asInstanceOf[Float]
    
            
  
    trait SimpleLog[T]:
        val debug = true
        def log(s : Any) = if debug then println(s)
        def loga(s : Any) : Unit = 
            val a : Exec[Unit] = Future(if debug then println(s))
            SimpleLog.run(a)
            
    object SimpleLog:
        class Impl[T] extends SimpleLog[T]
       
        import scala.concurrent.ExecutionContext.Implicits.given
        def run(e : Exec[Unit]) = e.onComplete{
            case _ => println("LOG ASYN OK OU KO")
        }
        object RootLogger extends SimpleLog[Nothing]
        val all = collection.mutable.Map[ClassTag[_],SimpleLog[_]]()
        def apply[T](using ct : ClassTag[T]) : SimpleLog[T]= 
            println(ct)
            all.getOrElseUpdate(ct,Impl[T]()).asInstanceOf[SimpleLog[T]]

  
    extension (c : Char)
        def isSep: Boolean =  c.isSpaceChar
        def isEndLine: Boolean = c == '\n' || c == '\r' 
        def isWhite :Boolean = isSep || isEndLine
    enum Exper:
        case Empty
        case Val(a : Any)
        case Symbol(s : String)
        case Operation(l : Exper,s : Char,r : Exper)

        def conbineRight(e : Exper):Exper =
            val ret = (this,e) match
                case (Empty,a ) => a
                case (a,Empty) => a
                case (Operation(l,s,Exper.Empty),r) =>  Operation(l,s,r)
                case (l,Operation(Exper.Empty,s,Exper.Empty)) =>  Operation(l,s,Exper.Empty)
                case (l,Operation(Exper.Empty,s,r)) =>  
                    l match 
                        case ll@Operation(a ,'+', b) => Operation(a ,'+', Operation(b,s,r))
                        case ll@Operation(a ,'-', b) =>  Operation(a ,'-', Operation(b,s,r))
                        case _ =>  Operation(l,s,r)
                case (left,right) => 
                    left match
                        case Operation(l,s,Operation(li,si,Exper.Empty)) => Operation(l,s,Operation(li,si,right))
                        case _ => 
                            right match
                                case Operation(Operation(Exper.Empty,si,ri),s,r) => Operation(Operation(left,si,ri),s,r)
                                case op : Operation => println("----------"); println(this);(println(e));Exper.findLeftPalce(op)(left)
                                
                                case _ => println(this);(println(e));???
                    

                case _ => println(this);(println(e));???
            
            logger.log(s"Exper : l : $this") 
            logger.log(s"Exper : r : $e")  
            logger.log(s"Exper : res : $ret")     
            ret

        def conbineRight(e : PhraseElement)(lowPrio : Char *):Exper =
            val exp = Exper(e)
            val ret = (this,exp) match
                case (Empty,a ) => a
                case (a,Empty) => a
                case (a : (Val | Symbol),  Exper.Operation(_,s,r)) => Exper.Operation(a,s,r)
                case (l : Exper.Operation ,  a:  (Val | Symbol)) => Exper.findRightPalce(l)(a)
                case ( Operation(l,s,r),  Exper.Operation(Empty,sr,Empty)) => 
                    if lowPrio.contains(s) && lowPrio.contains(sr) then
                        r match
                            case Operation(rl,rop,rr) => 
                                if lowPrio.contains(rop) then
                                    Exper.Operation(l,s, Exper.Operation(r,sr,Empty))
                                else
                                    Exper.Operation(Exper.Operation(l,s, r),sr,Empty)    
                            case _ => Exper.Operation(Exper.Operation(l,s, r),sr,Empty) 
                    else if lowPrio.contains(s) then
                        Exper.Operation(l,s, Exper.Operation(r,sr,Empty))
                    else
                        Exper.Operation(Exper.Operation(l,s, r),sr,Empty)         
                case (l,Exper.Operation(_,s,r)) => Exper.Operation(l,s,r)
                case (Exper.Operation(l,s,_),r) => Exper.Operation(l,s,r)
                case (a,b) => println((a,b));???
        
            logger.log(s"pel : l : $this") 
            logger.log(s"pel : r : $exp")  
            logger.log(s"pel : res : $ret")     
            ret
        
        def explain():String=
            this match 
                case Val(v) => v.toString
                case Empty => ""
                case Symbol(a) => a
                case Operation(l,op,r) =>  s"(${l.explain()} $op ${r.explain()})"
                case e:  _ => throw IllegalStateException(s"$e not supported yet")
        def evaluate(using ctx : String => Float):Float = 
            this match 
                case Val(v) => v.toString.toFloat
                case Empty => Float.NaN
                case Symbol(a) => ctx(a)
                case Operation(l,op,r) =>  Exper.evaluate( l.evaluate ,op ,r.evaluate )
                case e:  _ => throw IllegalStateException(s"$e not supported yet")
        def evaluateVal:Float = 
            this match 
                case Val(v) => v.toString.toFloat
                case Empty => Float.NaN
                case Symbol(a) => throw IllegalStateException(s"can't have symbol during static eval : $a")
                case Operation(l,op,r) =>  Exper.evaluate( l.evaluateVal ,op ,r.evaluateVal )
                case e:  _ => throw IllegalStateException(s"$e not supported yet")

    


        def toFunction[V](using ctx : ToFunction[String,V]):(v : V) => Float = 
            this match 
                case Val(va) =>  (v) =>  va.toString.toFloat
                case Empty => (v) =>Float.NaN
                case Symbol(z) => ctx(z)
                case Operation(l,op,r) => v => Exper.evaluate( l.toFunction(v) ,op ,r.toFunction(v) )
                case e:  _ => throw IllegalStateException(s"$e not supported yet")

        


    object Exper:
        val + :PhraseElement.Symbol = PhraseElement.Symbol(0,'+') 
        val *  :PhraseElement.Symbol = PhraseElement.Symbol(0,'*')
        val / :PhraseElement.Symbol  = PhraseElement.Symbol(0,'/')
        val -  :PhraseElement.Symbol = PhraseElement.Symbol(0,'-')
       
        val `(` :PhraseElement.Symbol = PhraseElement.Symbol(0,'(')
        val `)`  :PhraseElement.Symbol= PhraseElement.Symbol(0,')')

        def findLeftPalce(e : Operation): Exper => Operation = 
            e.l match
                case Empty =>
                     le  =>  e.copy(l= le)
                case op :  Operation => 
                    le => e.copy(l=findLeftPalce(op)(le))
        def findRightPalce(e : Operation): Exper => Operation = 
            logger.loga("FIND R")
            logger.loga(s"FIND $e")
            val ret : Exper => Operation=  e.r match
                case Empty =>
                     le  =>  
                        val  rr : Operation = e.copy(r= le)
                        logger.loga(rr)
                        rr
                case op :  Operation => 
                    le => e.copy(r=findRightPalce(op)(le))
                case _ => ???

            logger.loga(s"RET $ret")
            ret

        def evaluate(left : Float,op : Char,right : Float):Float =
            op match 
                case '+' => left + right
                case '-' => left - right
                case '%' => left % right
                case '/' => left / right
                case '*' => left * right
                case _ => throw new UnsupportedOperationException(s"I can't do $left $op $right")
        def trim(p :List[PhraseElement]):List[PhraseElement] = 
            if p.isEmpty then p 
            else 
                val lastIsSep =  p.last.isInstanceOf[ PhraseElement.Separator]
                val first =  p.head.isInstanceOf[ PhraseElement.Separator]
                (first, lastIsSep)  match 
                    case(true,true)  => p.drop(1).dropRight(1)
                    case(true,false)  => p.drop(1)
                    case(false,true)  => p.dropRight(1)
                    case(false,false)  => p

        def apply(string : String,option : AssosType =  AssosType.Natural )(using OpenCLose) :Exper =
            option match 
                case AssosType.Natural => Node(string.toPhrase).toExpressionWithAsso()
                case AssosType.LeftRight => Node(string.toPhrase).toExpression()
                


        def apply(phrase : PhraseElement):Exper=
            phrase match
                case (PhraseElement.Word(_,s)) if s.toString.matches("[^\\d]+") =>   Exper.Symbol(s)
                case(PhraseElement.Word(_,s)) => Exper.Val(s)
                case PhraseElement.Symbol(_,s) => Exper.Operation(Exper.Empty,s,Exper.Empty)
                case _ => Exper.Empty
    

    enum PhraseElement(val pos : Int):
        case Separator(posp : Int,val str : String) extends PhraseElement(posp)
        case EndLine(posp : Int,val str : String)  extends PhraseElement(posp)
        case Word(posp : Int,val str : String)  extends PhraseElement(posp)
        case Symbol(posp : Int,val str : Char)  extends PhraseElement(posp)
        def rep : Char | String = 
            this match 
                case e :( Separator | Word | EndLine )=> 
                    e match 
                        case a : Separator => a.str
                        case a : Word => a.str
                        case a : EndLine => a.str
                case a : Symbol =>
                    a.str
        override def equals(r : Any): Boolean=
            r match
                case e : PhraseElement => 
                    
                     this.rep == e.rep
                case _ => false
           
        def add(c : Char)=
            this match
                    case Separator(i,v) => Separator(i,v:+c)
                    case EndLine(i,v) =>  EndLine(i,v:+c) 
                    case Symbol(i,v) => ???
                    case Word(i,v) => Word(i,v:+c)      
        def accept(using s : ExporeString) : Boolean=
          
            import s.given  
            val cO = s.i.readCharOption
            cO.map{
                c =>
                    this match
                    case e : Separator => c.isSep
                    case e : EndLine =>  c.isEndLine
                    case e : Symbol => false
                    case e : Word => c.isLetterOrDigit || c== '.'  
            } getOrElse false
  
    object PhraseElement:
        def apply(pos : Int,char : Char)=
            
         
            if char.isSep then
               
                 Separator(pos,char.toString)
            else if char.isEndLine then   
              
                EndLine(pos,char.toString)
            else if !char.isLetterOrDigit && char != '.' then  
              
                Symbol(pos,char)
            else 
            
                Word(pos,char.toString)


    case class ExporeString( var i : Int,string : String):
        given String = string

      
    class VarValue[T](var value : T)
    case class Result[T](val value: List[T])
    object Phrase:
        def apply(str : String):List[PhraseElement] =
            given String = str
            val first = 0.readChar
            var cur = PhraseElement(0,first)
          
            var result = Result[PhraseElement](Nil)
            given a : ExporeString = ExporeString(0,str)
            for(i <- 1 until str.length) do
                a.i = i
                
                if(cur.accept) then 
                    cur = cur.add(i.readChar) 
                  
                if !cur.accept  then
                    result = result.copy(value = result.value :+ cur)   
                    cur = PhraseElement(i,i.readChar) 
                       
                

            result = result.copy(value = result.value :+ cur)       
            result.value

            
                
            

    given (String => Exper.Symbol) = Exper.Symbol(_)
    given (String =>  Exper.Val) = e => Exper.Val(e.toFloat)

    extension (i : Int) (using s: String) 
        def readChar : Char = s.charAt(i)
        def readCharOption : Option[Char] = if i < lengthString then Some(readChar) else None
        def next = i + 1
        def previous = i - 1
        def lengthString :Int = s.length
        def end = i == (i.lengthString-1)

 


    type OpenCLose = (PhraseElement, PhraseElement)  
    enum AssosType:
        case Natural;case LeftRight
    extension (s : String)
        def toExpression(using OpenCLose) : Exper =  Exper(s,AssosType.LeftRight)
        def toExpressionWithAsso(using OpenCLose)  : Exper = s.toNode.toExpressionWithAsso()
        def toPhrase : List[PhraseElement] =  Phrase(s)
        def toFunction[T](option : AssosType = AssosType.Natural)(using ToFunction[String,T],OpenCLose)  : T => Float =  
            Exper(s,option).toFunction[T]
        def toNode(using OpenCLose) :Node.Root[PhraseElement] =
            val f :List[PhraseElement] =s.toPhrase 
            Node(f)