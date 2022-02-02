enum Expr:
  case Number(num: Int)
  case Sum(x: Expr, y: Expr)
  case Prod(x: Expr, y: Expr)
  case Var(name: String)
  case Let(name: String, rhs: Expr, body: Expr)

import Expr._

/**
  * mapping between variable name with defined value
  * */
def eval(e: Expr): Int = {
  def recur(e: Expr)(using env: Map[String, Int]): Int = e match {
    case Number(n) => n
    case Sum(x, y) => recur(x) + recur(y)
    case Prod(x, y) => recur(x) * recur(y)
    case Var(name) => env(name)
    case Let(name, rhs, body) => recur(body)(using env + (name -> recur(rhs)))
  }
  recur(e)(using Map())
}