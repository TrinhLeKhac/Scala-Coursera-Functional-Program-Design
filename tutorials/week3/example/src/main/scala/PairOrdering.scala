
case class Movie(title: String, rating: Int, duration: Int)
/**
  * Sorting by one cretia
  * mySort(movies)(m => m.duration)
  * */

def mySort[A, B](xs: List[A])(cretia: A => B)(implicit ord: myOrdering[B]): List[A] = {
  def insert(y: A, ys: List[A]): List[A] = ys match {
    case Nil => List(y)
    case y1::ys1 => if(ord.lt(cretia(y), cretia(y1))) y::ys else y1::insert(y, ys1)
  }
  xs match {
    case Nil => Nil
    case x1::xs1 => insert(x1, mySort(xs1)(cretia))
  }
}

/**
  * Sorting by 2 cretia
  * mySort(movies)(m => (m.rating, m.duration))
  * */

object PairOrdering:
  def main(args: Array[String]): Unit = {
    val movies: List[Movie] = List(
      Movie("Interstellar", 9, 169),
      Movie("Inglorious Basterds", 8, 140),
      Movie("Fight Club", 9, 139),
      Movie("Zodiac", 8, 157)
    )
//    val demo1 = mySort(movies)((m: Movie) => m.duration)
//    demo1.foreach(m => println(m.title))
    val demo2 = mySort(movies)((m: Movie) => (m.rating, m.duration))
    demo2.foreach(m => println(m.title))
  }
