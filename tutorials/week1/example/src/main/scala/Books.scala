
trait Books

object Books:
    case class Book(title: String, authors: List[String]) extends Books
//    val selectedBooks = 
//        for 
//            book <- Data.books
//            authors <- book.authors
//            if(authors.startsWith("Bird, "))
//        yield book.title
//    println(selectedBooks)
