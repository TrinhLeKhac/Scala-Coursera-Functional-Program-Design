
object Data:
    import Books._
    val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Progamming", 
    authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java", 
    authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers", 
    authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala", 
    authors = List("Odersky, Martin", " Spoon, Lex", "Venners, Bill"))
)
    val js = JSON.Obj(Map(
                        "firstName" -> JSON.Str("John"),
                        "lastName" -> JSON.Str("Smith"),
                        "address" -> JSON.Obj(Map(
                            "streetAddress" -> JSON.Str("21 2nd Street"),
                            "state" -> JSON.Str("NY"),
                            "postalCode" -> JSON.Num(10021)
                        )),
                        "phoneNumber" -> JSON.Seq(List(
                            JSON.Obj(Map(
                                "type" -> JSON.Str("home"),
                                "number" -> JSON.Str("212 555-1234")
                            )),
                            JSON.Obj(Map(
                                "type" -> JSON.Str("fax"),
                                "number" -> JSON.Str("123 555-1234")
                            ))
                        ))
                    ))
end Data
