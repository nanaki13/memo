package bon.jo.memo

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import bon.jo.memo.CommandLineParser.OptionName

object CommandLineParser :
    class CommandLineParserEx(val keyNotFoud : List[OptionName] ) extends Exception(s"key not found : ${keyNotFoud.mkString(", ")}")
    opaque type OptionName = String
    object OptionName:
        def apply(s : String) : OptionName = s
        def unapply(s : OptionName) : String = s

    opaque type Value = String

    object Value:
        def apply(s : String) : Value = s
        def unapply(s : Value) : String = s
     
    extension (s : String)
        def toOptionName = OptionName(s)
        def toValue = Value(s)

    def parseOptionName(str : String) = str.toList match
        case '-' +: '-' +: option => OptionName(option.mkString)
        case '-' +: option  => OptionName(option.mkString)
        case _ => OptionName(str)
    case class Result(r : OptionName => scala.Option[Value] ):
        def apply(opts : OptionName*): Iterable[(OptionName,Option[Value])] = opts map (o => (o,r(o)))
        def failIfNot(opts : OptionName*): Try[List[(OptionName,Value)]] =  
          
            val tr: List[(OptionName,Option[Value])] = this(opts : _ *).toList
          
            if tr.exists(_._2.isEmpty)
            then
                tr.filter(_._2.isEmpty).foldLeft(Failure(CommandLineParserEx(Nil))){
                    case (Failure(h : CommandLineParserEx),e) => Failure(CommandLineParserEx(h.keyNotFoud :+ e._1))
                    case (a@Failure(_),_) => a
                }
            else
                tr.foldLeft[Try[List[(OptionName,Value)]]](Success(Nil)){
                    case (Success(h ), e  ) => Success(h  :+ (e._1,e._2.get))
                    case (a,_) => a
                }
         
        end failIfNot
  
    def apply(args : Array[String]): Result = 
        Option(args) match
            case Some(v )=>
                val flattenList : List[OptionName | Value] = (for v <- args.zipWithIndex
                    yield 
                        val (valueOrOptionName,index) = v
                        if index % 2 == 0 
                        then parseOptionName(valueOrOptionName)
                        else Value(valueOrOptionName)
                    ).toList
                        Result((e : OptionName) => flattenList.zipWithIndex.find(_._1 == e).flatMap(
                e => 
                    if e._2+1 < flattenList.size
                    then Some(flattenList(e._2+1).asInstanceOf[Value])
                    else None
                )
            )
            case _ => Result(_ => None)
        


    end apply 



