package example
import Signal._
/**
 * Reactive programming
 * reacting to sequences of events that happen in time
 *
 * Function view:
 * Aggregate an event sequence into a signal that have value change overtime
 *
 * */


/**
 *  event-based view:
 *  whenever mouse moves, an event
 *  MouseMoved(toPos: Position) is fired
 *
 *  FRP view:
 *  A signal
 *  mousePosition: Signal[Position]
 *  which at any point in time represents the current mouse position
 * */


/**
 * class BankAccount
 *
 * */

import Signal._

class BankAccount:
  def balance: Signal[Int] = myBalance

//  private var myBalance: Int = 0
  private val myBalance = Signal.Var[Int](0)

  def deposit(amount: Int) =
    if(amount > 0)
      val b = myBalance()
      myBalance() = b + amount

  def withdraw(amount: Int): Int =
    if((0 < amount) && (amount <= balance()))
      val b = myBalance()
      myBalance() = b - amount
      myBalance()
    else throw Error("insufficient money")
end BankAccount

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)


object Application:
  def main(args: Array[String]): Unit = {
//    var x: Int = 1
//    var y: Int = x * 2
//    x = 45
//    println(y)

//    val sig = Signal.Var(3)
//    sig.update(5)
//    println(sig())

//    val x = Signal.Var(1)
//    val y = Signal.Var(x()*2)
//    /**
//     * change status of x*/
////    x = Signal.Var(3)
//    x() = 3
//    println(y())
    /**
     * arr(i) = 0 <=> arr.update(i, 0)
     *
     * sig.update(5) <=> sig() = 5
     * */

    val a = BankAccount()
    val b = BankAccount()
    val c = consolidated(List(a, b))
    println(c())

    a.deposit(10)
    println(c())
    b.deposit(10)
    println(c())
    a.withdraw(8)
    println(c())
  }
