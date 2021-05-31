package bon.jo.memo

import bon.jo.rpg.stat.Perso
import scala.compiletime.ops.string
import bon.jo.rpg.stat.Actor
import bon.jo.rpg.stat.BaseState
import scala.util.Random

object Script:
    trait ToFunction[V,T] :
        def apply(v : V): T => Float

  
    extension (c : Char)
        def isSep: Boolean =  c.isSpaceChar
        def isEndLine: Boolean = c == '\n' || c == '\r' 
        def isWhite :Boolean = isSep || isEndLine
    enum Exper:
        case Empty
        case Val(a : Any)
        case Symbol(s : String)
        case Operation(l : Exper,s : Char,r : Exper)

        def evalFloat(using ctx : String => Float):Float = 
            this match 
                case Val(a) => a.toString.toFloat
                case Empty => Float.NaN
                case Symbol(a) => ctx(a)
                case Operation(l,'+',r) => l.evalFloat+r.evalFloat
                case Operation(l,'*',r) => l.evalFloat*r.evalFloat
             
                case e:  _ => throw IllegalStateException(s"$e not supported yet")
        def toFunction[V](using ctx : ToFunction[String,V]):(v : V) => Float = 
            this match 
                case Val(a) => (v) => a.toString.toFloat
                case Empty => (v) =>Float.NaN
                case Symbol(a) => ctx(a)
                case Operation(l,'+',r) => (v) => l.toFunction[V](v)+r.toFunction[V](v)
                case Operation(l,'*',r) => (v) =>  l.toFunction[V](v)*r.toFunction[V](v)
              
                case e:  _ => throw IllegalStateException(s"$e not supported yet")
        def toFunctionSymbol[V](using ctx : ToFunction[Exper.Symbol,V]):(v : V) => Float = 
            this match 
                case Val(a) => (v) => a.toString.toFloat
                case Empty => (v) =>Float.NaN
                case a : Symbol => ctx(a)
                case Operation(l,'+',r) => (v) => l.toFunctionSymbol[V](v)+r.toFunctionSymbol[V](v)
                case Operation(l,'*',r) => (v) =>  l.toFunctionSymbol[V](v)*r.toFunctionSymbol[V](v)
            
                case e:  _ => throw IllegalStateException(s"$e not supported yet")
    object Exper:
        val + = PhraseElement.Symbol('+') 
        val * = PhraseElement.Symbol('*')
        val / = PhraseElement.Symbol('/')
        val - = PhraseElement.Symbol('-')
        val operation : Set[PhraseElement] = Set(Exper.+,*,/,Exper.- )
        val `(` = PhraseElement.Symbol('(')
        val `)` = PhraseElement.Symbol(')')
        def trim(p :List[IPhraseElement]):List[IPhraseElement] = 
            if p.isEmpty then p 
            else 
                val lastIsSep =  p.last._2.isInstanceOf[ PhraseElement.Separator]
                val first =  p.head._2.isInstanceOf[ PhraseElement.Separator]
                (first, lastIsSep)  match 
                    case(true,true)  => p.drop(1).dropRight(1)
                    case(true,false)  => p.drop(1)
                    case(false,true)  => p.dropRight(1)
                    case(false,false)  => p
        type IPhraseElement = (Int,PhraseElement)
        def apply(string : String):Exper = Exper(Phrase(string))
        def apply(phrase : List[IPhraseElement]):Exper=
                val clean =  trim(phrase) 
                val withoutIndex=  phrase.map(_._2)
                val inP = withoutIndex.indexOf(`(`)
                val containExp =  inP != -1
                val plusBeforeExp = containExp && withoutIndex.indexOf(Exper.+) < inP
                clean match
                    case Nil => Exper.Empty
                   
                    case (i, `(`) :: tail =>
                        val end = tail.map(_._2).lastIndexOf(`)`)
                        
                        if end != -1 then
                            val (left,right) = tail.splitAt(end)
                       
                         
                            val rTrim = trim(right.tail)
                            if rTrim.nonEmpty then
                               
                                if operation.contains(rTrim.head._2) then
                                    Exper.Operation(Exper(left),rTrim.head._2.asInstanceOf[PhraseElement.Symbol].str,Exper(rTrim.tail))
                                else
                                    Exper(left)
                            else
                                Exper(left)
                        else
                            throw new IllegalStateException(s"not ) find for ( at index $i")
                     

                    case head :: Nil => println(head) ;Exper(head) 

                    case l : List[IPhraseElement] if l.map(_._2).contains(Exper.+) && (plusBeforeExp || !containExp )=>
                       
                        val sep = l.map(_._2).indexOf(PhraseElement.Symbol('+'))
                        val (le,r) = l.splitAt(sep)
                  
                        Exper.Operation(Exper(le),'+',Exper(r.tail))
                    case l : List[IPhraseElement] if l.map(_._2).contains(PhraseElement.Symbol('*')) =>
                       
                        val sep = l.map(_._2).indexOf(PhraseElement.Symbol('*'))
                        println(s"* l = ${ (l.map(_._2))}")
                        val (le,r) = l.splitAt(sep)
                        println(s"* left = ${ (le)}")
                        println(s"* right = ${ (r)}")
                        Exper.Operation(Exper(le),'*',Exper(r.tail))

                    case z => println(s"?? => $z") ; Exper.Empty
                   

            
        def apply(phrase : IPhraseElement):Exper=
            phrase match
                case (_,PhraseElement.Word(s)) if s.toString.matches("[^\\d]+") =>   Exper.Symbol(s)
                case(_,PhraseElement.Word(s)) => Exper.Val(s)
                case _ => Exper.Empty
    

    enum PhraseElement:
        case Separator(str : String)
        case EndLine(str : String)
        case Word(str : String)
        case Symbol(str : Char)
        def add(c : Char)=
            this match
                    case Separator(v) => Separator(v:+c)
                    case EndLine(v) =>  EndLine(v:+c) 
                    case Symbol(v) => ???
                    case Word(v) => Word(v:+c)      
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
        def apply(char : Char)=
            
         
            if char.isSep then
               
                 Separator(char.toString)
            else if char.isEndLine then   
              
                EndLine(char.toString)
            else if !char.isLetterOrDigit && char != '.' then  
              
                Symbol(char)
            else 
            
                Word(char.toString)


    case class ExporeString( var i : Int,string : String):
        given String = string

      
    class VarValue[T](var value : T)
    case class Result[T](val value: List[T])
    object Phrase:
        def apply(str : String):List[(Int,PhraseElement)] =
            given String = str
            val first = 0.readChar
            var cur = PhraseElement(first)
          
            var result = Result[(Int,PhraseElement)](Nil)
            given a : ExporeString = ExporeString(0,str)
            for(i <- 1 until str.length) do
                a.i = i
                
                if(cur.accept) then 
                    cur = cur.add(i.readChar) 
                  
                if !cur.accept  then
                    result = result.copy(value = result.value :+ (i,cur))   
                    cur = PhraseElement(i.readChar) 
                       
                

            result = result.copy(value = result.value :+ (str.length-1,cur))       
            result.value

            
                
            

    def apply(str : String): Perso => Any = ???
     //   parse(str)


    given (String => Exper.Symbol) = Exper.Symbol(_)
    given (String =>  Exper.Val) = e => Exper.Val(e.toFloat)
   // given (String =>  Exper.Operation) = e => Exper.Operation(e.toFloat)

    extension (i : Int) (using s: String) 
        def readChar : Char = s.charAt(i)
        def readCharOption : Option[Char] = if i < lengthString then Some(readChar) else None
        def next = i + 1
        def previous = i - 1
        def lengthString :Int = s.length
        def end = i == (i.lengthString-1)

 


        
    extension (s : String)
        def toExpression : Exper =  Exper(s)
        def toPhrase : List[(Int,PhraseElement)] =  Phrase(s)
        def toFunction[T](using ToFunction[Exper.Symbol,T])  : T => Float =  Exper(s).toFunctionSymbol[T]

@main def t = 
    import Script.*
  


    val act = Perso(1,"df","fsqf", BaseState.`1`,10,10,Nil,None,None)
    val test = """a.hp + rand"""
    val str = """a.str + a.hp"""
    val map = Map[Exper.Symbol,(Actor => Float)](
        Exper.Symbol("a.hp") -> ( _.stats.hp.toFloat),
         Exper.Symbol("a.str") -> ( _.stats.str.toFloat),
         Exper.Symbol("rand") -> ( _ => Random.nextFloat())
        
        )
    given ToFunction[String,Actor] = str => act => str match
        case "a.hp" => act.stats.hp.toFloat
        case "a.res" => act.stats.res.toFloat
        case "a.str" => act.stats.str.toFloat
        case _ => 0
    given  ToFunction[Exper.Symbol,Actor] = s => map(s)
    //given (String => Float) = act.stats.map(_.toFloat).toNameValueList.map(((k,v) => ("a."+k,v))).toMap
    
    var ff = test.toFunction[Actor]

    println(ff(act))

    val ff2 = str.toFunction[Actor]

    println(ff2(act))
    println("(A + B)".toExpression)
    println("A*A + A".toExpression.evalFloat(using (e) => 2f))
    case class A(v : Float)
    given  ToFunction[Exper.Symbol,A] = str => a => a.v
    val f : A => Float = "A*A + A".toFunction[A]

    println(f(A(4)))

     