package bon.jo.common

enum Tree[A]:
    case Value( a : A)
    case Mix( a : Value[A],list : List[Tree[A]])
    case OnlyTree(list : List[Tree[A]])
    def addChildValue(a : A):Tree[A]=
        this match 
            case h@Value(_) =>  OnlyTree(List(h,Value(a)))
            case Mix(v,c) =>
                 Mix(v,c:+Value(a))
            case OnlyTree(c) => OnlyTree(c:+Value(a))
    def value(a : A):Tree[A] =
        this match
            case Value(_) =>  Value(a)
            case Mix(_,c)  if c.nonEmpty =>  Mix( Value(a),c)
            case Mix(v,_) => Value(a)
            case OnlyTree(c) if c.nonEmpty => Mix( Value(a),c)
            case OnlyTree(_) =>  Value(a)
    def childs(a : Tree[A]*):Tree[A] = 
        this match
            case h@Value(_) =>  Mix(h,a.toList)
            case Mix(v,_) => Mix(v,a.toList)
            case OnlyTree(_) => OnlyTree(a.toList)
    /*def reduceE[B](using
        vMapper : Value[A] => B)(using Add[B]):B=
            this match
                case h:Value[A] => vMapper(h)
                case Mix(v,childs)=>
                    val ret =  vMapper(v)
                    val childsWrap =  Add().childs( childs.map(_.reduce))  
                    ret :+ childsWrap
                    ret
                case OnlyTree(childs) => 
                    Add().childs( childs.map(_.reduce))*/

                 
end Tree
object Add:
    def apply[A]()(using Add[A]) = summon
trait Add[A]:
    def add(par : A,c :A):Unit
    def monoid:A
    def childs(childsp : List[A]):A = 
        val a = monoid  
        childsp.foreach(a :+ _)
        a
    extension (a : A)
        def :+(b:A):Unit = add(a,b)
        def +(b:A):A = 
            val ret = monoid
            ret :+ a
            ret :+ b
            ret
object Tree:
    given Functor[Tree] with
        extension [A](a : Tree[A])
            def map[B](f : A => B):Tree[B]=
                a match 
                    case Value(h) => Value[B](f(h))
                    case Mix(v,h) =>  Mix[B](v.map(f).asInstanceOf[Value[B]],h.map(e => e.map(f)))
                    case OnlyTree(h) => OnlyTree[B](h.map(e => e.map(f)))
            def reduce(using Add[A]):A=
                a match
                    case e : Value[A] => e.a
                    case v : Mix[A]=>
                        val ret : A =  v.a.a
                        val childsWrap =  Add().childs( v.list.map(e =>e.reduce))  
                        ret :+ childsWrap
                        ret
                    case v : OnlyTree[A] => 
                        Add().childs( v.list.map(e =>e.reduce))

    class Ref[A](var ref : Tree[A] )
    type T[A] = Ref[A] ?=>Ref[A]

    def /[A](e : T[A]):Tree[A] =
        given Ref[A] = Ref(Tree.OnlyTree(Nil))
        e.ref
    def applyTo[A](f: Tree[A]=> Tree[A]): Ref[A] ?=> Ref[A]=
        val ctx :  Ref[A] = summon
        ctx.ref = f(ctx.ref)
        ctx
    def value[A](a:A):T[A]=
        applyTo[A](_.value(a))

    def addChildValue[A](a : A):T[A]=
        applyTo[A](_.addChildValue(a))

    def childs[A](a :  Tree[A] *):T[A]=
         applyTo[A](_.childs(a :_ *))
