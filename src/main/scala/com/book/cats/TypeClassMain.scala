package com.book.cats
import JsonWriterInstances._
import JsonSyntax._
final case class Person(name: String, email: String)

object TypeClassMain {
  def main(args: Array[String]): Unit = {
    println(Json.toJson(Person(name = "name", email = "email")))
    println(Person(name = "name", email = "email").toJson)
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