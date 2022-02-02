
def mysort(xs: List[Int]): List[Int] = {
  def insert(y: Int, ys: List[Int]): List[Int] = ys match {
    case Nil => List(y)
    case y1 :: ys1 => if(y > y1) y1::insert(y, ys1) else y::ys
  }
  xs match {
    case Nil => Nil
    case x1::xs1 => insert(x1, mysort(xs1))
  }
}

//def myUpgradeSort[A](xs: List[A])(order: (A, A) => Boolean): List[A] = {
def myUpgradeSort[A](xs: List[A])(implicit order: myOrdering[A]): List[A] = {
  def insert(y: A, ys: List[A]): List[A] = ys match {
    case Nil => List(y)
    case y1::ys1 => if(order.lt(y, y1)) y::ys else y1::insert(y, ys1)
  }
  xs match {
    case Nil => Nil
    case x1::xs1 => insert(x1, myUpgradeSort(xs1)(order))
  }
}

/**
  * scala.math have trait Ordering
  * import scala.math
  * trait Ordering[A]:
  * def compare(a: A, b: A): Int(1 if a > b, -1 if a < b else 0)
  * def lt(a: A, b: A): Boolean = compare(a, b) <= 0
  *
  * */

trait myOrdering[A]:
  def compare(a: A, b: A): Int
  def lt(a: A, b: A) = compare(a, b) < 0
  def lteq(a: A, b: A) = compare(a, b) <= 0
  def gt(a: A, b: A) = compare(a, b) > 0
  def gteq(a: A, b: A) = compare(a, b) >= 0

object myOrdering: // in companion object using new myOrdering[Int] okie
  implicit def Int: myOrdering[Int] = new myOrdering[Int] {
    def compare(a: Int, b: Int) = if(a > b) 1 else if(a < b) -1 else 0
  }
  implicit def String: myOrdering[String] = new myOrdering[String] {
    def compare(a: String, b: String) = a.compareTo(b)
  }
  implicit def orderingPair(implicit ord: myOrdering[Int]): myOrdering[(Int, Int)] =
    new myOrdering[(Int, Int)] {
    def compare(a: (Int, Int), b: (Int, Int)): Int =
      val firstCriteria = ord.compare(a._1, b._1)
      if(firstCriteria != 0) firstCriteria
      else
        val secondCriteria = ord.compare(a._2, b._2)
        secondCriteria
  }
  implicit def orderingList(implicit ord: myOrdering[Int]): myOrdering[List[Int]] =
    new myOrdering[List[Int]] {
      def compare(a: List[Int], b: List[Int]): Int = (a, b) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => 1
        case (a1::as, b1::bs) => {
          val compareValue = ord.compare(a1, b1)
          if(compareValue != 0) compareValue else compare(as, bs)
        }
      }
    }

//trait myOrderingPair[A]:
//  def compare(a: (A, A), b: (A, A)): Int
//  def lt(a: (A, A), b: (A, A)): Boolean = compare(a, b) < 0

//object myOrderingPair:
//  implicit def Int: myOrderingPair[Int] = new myOrderingPair[Int] {
//    def compare(a: (Int, Int), b: (Int, Int)): Int = {
//      if (a._1 > b._1) 1
//      else if (a._1 < b._1) -1
//      else if (a._2 > b._2) 1
//      else if (a._2 < b._2) -1
//      else 0
//    }
//  }
//  implicit def String: myOrderingPair[String] = new myOrderingPair[String] {
//    def compare(a: (String, String), b: (String, String)): Int = {
//      if(a._1.compareTo(b._1) > 0) 1
//      else if(b._1.compareTo(a._1) > 0) -1
//      else
//        if(a._2.compareTo(b._2) > 0) 1
//        else if(b._2.compareTo(a._2) > 0) -1
//        else 0
//      }
//    }

object Orderings:
  def main(args: Array[String]): Unit = {
    val ls = List(4, 1, 2)
    val sortList = mysort(ls)
    sortList.foreach(println)

    val lss = List("trinhlk2", "yippie", "truong")
//    val sortLss = myUpgradeSort(lss)((x: String, y: String) => x < y)
//    val sortLss2 = myUpgradeSort(lss)(myOrdering.String) // using new myOrdering[String] fails
    val sortLss2 = myUpgradeSort(lss) // error no implicit argument of type myOrdering[String] ??? => fix(implicit def Int, String)
    sortLss2.foreach(println)

    val xss = List(List(1, 2, 3), List(1, 1, 3), List(1))
    val res = myUpgradeSort(xss)
    res.foreach(ls => ls.foreach(println))
  }

