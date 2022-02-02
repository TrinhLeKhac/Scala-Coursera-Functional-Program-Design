
trait Method

extension[T](xs: List[T])

    def map[U](f: T => U): List[U] = xs match {
        case Nil => Nil
        case x :: xs1 => f(x) :: xs1.map(f)
    }
    def mapFunc[U](f: T => U): List[U] = for(x <- xs) yield f(x)

    def flatMap[U](f: T => List[U]): List[U] = xs match {
        case Nil => Nil
        case x :: xs1 => f(x) ++ xs1.flatMap(f)
    }
    def flatMapFunc[U](f: T => List[U]): List[U] = for(x <- xs; y <- f(x)) yield y

    def filter(p: T => Boolean): List[T] = xs match {
        case Nil => Nil
        case x :: xs1 => if(p(x)) x :: xs1.filter(p) else xs1.filter(p)
    }
    def filterFunc(p: T => Boolean): List[T] = for(x <- xs; if(p(x))) yield x