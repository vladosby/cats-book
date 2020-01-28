package com.book.cats
import JsonWriterInstances._
import JsonSyntax._
import RecursiveResolution._

final case class Person(name: String, email: String)

final case class Person2(name: Option[String], email: Option[String])

object TypeClassMain {
  def main(args: Array[String]): Unit = {
    println(Json.toJson(Person(name = "name", email = "email")))
    println(Person(name = "name", email = "email").toJson)
    println(Option(Person(name = "name", email = "email")).toJson)
    println(Option.empty[String].toJson)
  }
}

sealed trait Json

case class JsObject(get: Map[String, Json]) extends Json

case class JsString(get: String) extends Json

case class JsonNumber(get: Double) extends Json

case object JsNull extends Json

// interface
trait JsonWriter[A] {
  def write(value: A): Json
}

//instances
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] = (value: Person) => JsObject(Map("name" -> JsString(value.name), "email" -> JsString(value.email)))
}

// interfaces

// interface object
object Json {
  def toJson[A: JsonWriter](value: A): Json = implicitly[JsonWriter[A]].write(value)
}

//interface syntax
object JsonSyntax {
  implicit class JsonWriteOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}

//recursive implicit resolution
object RecursiveResolution {
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
    override def write(value: Option[A]): Json = {
      value match {
        case Some(v) => writer.write(v)
        case None => JsNull
      }
    }
  }
}


// exercise
trait Printable[A] {
  def format(v: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (v: String) => s"printable string: $v"
  implicit val intPrintable: Printable[Int] = (v: Int) => s"printable int: $v"
  implicit val catPrintable: Printable[Cat] = (v: Cat) => s"${v.name} is a ${v.age} year-old ${v.color} cat."
}

object Printable {
  def format[A : Printable](v: A): String = {
    implicitly[Printable[A]].format(v)
  }

  def print[A : Printable](v: A): Unit = {
    println(implicitly[Printable[A]].format(v))
  }
}

final case class Cat(name: String, age: Int, color: String)

object PrintableMain {
  def main(args: Array[String]): Unit = {
    import PrintableInstances._

    Printable.print("Hello")
    Printable.print(123)

    val cat = Cat(name = "Cat name", age = 12, color = "blue")
    Printable.print(cat)
  }
}