package example

class BankAccount {
  private var amount: Int = 0
  def deposit(n: Int): Unit = if(n > 0) amount = amount + n
  def withdrawn(n: Int): Int = {
    if((0 < n) && (n < amount)) {amount = amount - n; amount}
    else
      throw Error("insufficient money")
  }
}

//trait TailLayList[T]:
//  def head: T
//  def tail: TailLayList[T]
//
//object TailLayList:


object Application:
  def main(args: Array[String]): Unit = {
    val bank = new BankAccount
    bank.deposit(50)
    /**
     * the same function and parameters
     * bank.withdrawn(20)
     * the result is different
     *  => object bank is fully state, have side-effect
     *  fully state(have side-effect) based on var amount
     *  */
//    println(bank.withdrawn(20))  // 30
//    println(bank.withdrawn(20))  // 10
//    bank.withdrawn(20)  // throw error

    /**
     * referential transparent
     * val x = E
     * val y = E
     *
     * val x = E
     * val y = x
     *
     * if no possible test can distinguish x and y
     * => referential transparent
     * */

//    val x = new BankAccount
//    val y = new BankAccount
//    x.deposit(30) // 30
//    y.withdrawn(20) // error

    val x = new BankAccount
    val y = x
    x.deposit(30) // 30
    y.withdrawn(20) // 10

    /**
     * in this case substition model not work
     * substition model: can replace name of value by its expression
     * val x = E
     * val y = E
     *
     * val x = E
     * val y = x
     * we can replace name of value by its expression
     * therefore can replace x by E
     * => y = E
     *
     * but (val x = E, val y = E) different with (val x = E, val y = x)
     * => substition model not working
     * */
  }
