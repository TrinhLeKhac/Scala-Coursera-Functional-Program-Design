/**
  * data structure with map and flatMap is common
  * describes class data structure with some algebraic laws => monads
  *
  * define monads:
  * monad M is a parametric type M[T] with two operations flatMap and map, satisfies some laws as below
  * */

trait M[+T]
extension[T, U](m: M[T])
  def flatMap(f: T => M[U]): M[U]
