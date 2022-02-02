/**
 * Java Script Object Notation(JSON)
 * {"firstName": "John",
    "lastName": "Smith",
    "address": {
        "streetAddress": "21 2nd Street",
        "state": "NY",
        "postalCode": 10021
    },
    "phoneNumber": [
        {"type": "home", "number": "212 555-1234"},
        {"type": "fax", "number": "646 555-4567"}
    ]
} 
* modelling data by Scala's way: case class
* case class modelling data group in companion object
* trait/ abstract class JSON(no method and field)
* companion object contains case class extends from trait/abstract class
* case class extends abstract class/trait => have constructor contains trait/case class
* 
* primitive json: => boolean, number, null, string
* complicated json: => Map[String, JSON], List[JSON]
*/

abstract class JSON
object JSON:
    case class Seq(elem: List[JSON]) extends JSON
    case class Obj(elem: Map[String, JSON]) extends JSON
    case class Num(n: Int) extends JSON
    case class Str(s: String) extends JSON
    case class Bool(b: Boolean) extends JSON
    case object Null extends JSON

    def show(json: JSON): String = json match {
        case JSON.Bool(b) => b.toString
        case JSON.Num(n) => n.toString
        case JSON.Str(s) => dquote(s)
        case Null => "null"
        case JSON.Obj(bindings) => {
        val maps = bindings.map((str, json) => s"${dquote(str)}: ${show(json)}")
        maps.mkString("{\n", ",\n", "\n}")
        }
        case JSON.Seq(elem) => {
        val maps = elem.map(show)
        maps.mkString("[\n", ", \n", "]")
        }
    }
    def dquote(s: String): String = s"$"$s$""

    def bindings(js: JSON): List[(String, JSON)] = js match {
        case JSON.Obj(bindings) => bindings.toList
        case _ => Nil
    }

end JSON
