import java.util.concurrent.TimeUnit
import scala.concurrent.duration.TimeUnit

sealed trait Json
case class JNumber(value: Int) extends Json
case class Jstring(value: String) extends Json
case class JBoolean(value: Boolean) extends Json
case class JArray(value: List[Json]) extends Json
case class JObject(value: (String, Json)*) extends Json

object Json{
  import scala.language.implicitConversions
  implicit def stringToJson(s: String): Json = Jstring(s)
  implicit def numberToJson(n: Int): Json = JNumber(n)
}

/**
  * improve style => val js = JObject("name" -> "John", "age" -> 42)
  *
  * def obj(fields: (String, Json)*): Json = JObject(fields:_*)
  *
  * implicit def
  * */
def obj(fields: (String, Json)*): Json = JObject(fields:_*)
/**
  *
  * */
case class Duration(value: Int, unit: TimeUnit)
implicit class convert(n: Int) {
  def seconds: Duration = Duration(n, TimeUnit.SECONDS)
}
/**
  *
  * */
implicit class even(n: Int) {
  def isEven: Boolean = n % 2 == 0
}

object AST{
  def main(args: Array[String]): Unit = {
    // {"name": "John", "age": 42}
    val js = JObject("name" -> Jstring("John"), "age" -> JNumber(42))
    // change method to defind JObject => implicit conversion from "string" or Int to JString, JNumber
    val demoJs = JObject("name" -> "John", "age" -> 42)

    val delay = Duration(15, TimeUnit.SECONDS)
    println(delay.value)
    // want to write delay = 15.seconds
    // 15.seconds => new className(15).seconds(className is implicit class)

    val delayDemo = 15.seconds
    println(delayDemo.value)

    println(42.isEven) // new className(42).isEven(implicit className)
  }
}

