
/**
  *
  * */
case class Person(name: String)
case class Paper(title: String, authors: List[Person], body: String)

object ConfManagement:
  /**
    * Viewers only inside of ConfManagement
    * */
  opaque type Viewers = Set[Person]
  /**
    * syntactic sugar
    * */
  def viewers(using vs: Viewers): Viewers = vs
  /**
    * define type
    * trade type for parameters
    * */
  type Viewed[T] = Viewers ?=> T
  /**
    * now we can replace all type (using Viewers): SomeType
    * with Viewed[SomeType]
    * */

  class Conference(ratings: (Paper, Int)*):

    private val realScore = ratings.toMap

    def papers: List[Paper] = ratings.map(_._1).toList

//    def score(paper: Paper)(using viewers: Viewers): Int = {
    def score(paper: Paper): Viewed[Int] = {
      if(paper.authors.exists(viewers.contains)) -100
      else realScore(paper)
    }

//    def rankings(using Viewers): List[Paper] = {
    def rankings: Viewed[List[Paper]] = {
      papers.sortBy(score(_)).reverse
    }
  /**
    * literal function with implicit parameters(?=>)
    * */
//    def rankings = (viewers: Viewers) ?=> papers.sortBy(score(_)).reverse

    def ask[T](p: Person, query: Viewed[T]) = query(using Set(p))

    def delegateTo[T](p: Person, query: Viewed[T]): Viewed[T] = query(using viewers + p)

  end Conference
end ConfManagement

object Application:
  def main(args: Array[String]): Unit = {
    import ConfManagement.*

    val Smith = Person("Smith")
    val Peters = Person("Peters")
    val Abel = Person("Abel")
    val Black = Person("Black")
    val Ed = Person("Ed")

    val conf = new Conference(
      Paper("How to grow beans", List(Smith, Peters), "...") -> 92,
      Paper("Organic gardening", List(Abel, Peters), "...") -> 83,
      Paper("Composting done right", List(Black, Smith), "...") -> 99,
      Paper("The secret life of snails", List(Ed), "...") -> 77
    )
    def highlyRankedProlificAuthors(asking: Person): Set[Person] = {
      def query: Viewed[Set[Person]] = {
//        given Viewers = viewers
        val highlyRanked = conf.rankings.takeWhile(conf.score(_) > 80).toSet
          for
            p1 <- highlyRanked
            p2 <- highlyRanked
            author <- p1.authors
            if((p1 != p2) && (p2.authors.contains(author)))
          yield author
      }
      conf.ask(asking, query)
    }
    def testAs(person: Person) = highlyRankedProlificAuthors(asking = person).map(_.name).mkString(", ")

    val res1 = testAs(Person("Black"))
    val res2 = testAs(Person("Smith"))
    val res3 = testAs(Person("Abel"))
    val res4 = testAs(Person("Ed"))
    println(res4)
  }