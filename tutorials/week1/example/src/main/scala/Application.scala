import JSON.{bindings, show}

object Application {
  def main(args: Array[String]): Unit = {
    println("Run for case book - 1")
    val selectedBooks =
      for
        book <- Data.books
        authors <- book.authors
        if(authors.startsWith("Bird, "))
      yield book.title
    println(selectedBooks)
    println(s"*******************************************************")

    println("Run for case book - 2")
    val selectedBooks2 =
      for
        book <- Data.books
        if(book.title.contains("Program"))
      yield book.title
    println(selectedBooks2)
    println(s"******************************************************")

    println("Run for case book - 3")
    println("Find name of authors who have writen at least 2 books")
    val selectedAuthors =
      for
        // bs1 <- Data.books
        bs1 <- Data.books.toSet
        bs2 <- Data.books
        if(bs1.title < bs2.title)
        author1 <- bs1.authors
        author2 <- bs2.authors
        if(author1 == author2)
      yield author1
    println(selectedAuthors)
    // println(selectedAuthors.toSet)
    println(s"*************************************************************************")

    println("Run for case json")
    val parses = show(Data.js)
    println(parses)

    println("Testing for yield with case")
    val filterNumber =
      for
        case ("phoneNumber", JSON.Seq(phoneInformations)) <- bindings(Data.js)
        phoneInfor <- phoneInformations
        case ("number", JSON.Str(number)) <- bindings(phoneInfor)
        if number.startsWith("212")
      yield number
    println(filterNumber)
    println(s"***************************************************************")
  }
}
