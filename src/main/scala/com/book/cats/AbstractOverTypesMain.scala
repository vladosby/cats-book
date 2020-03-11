package com.book.cats

import cats.{Applicative, Id}
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds
import cats.syntax.functor._

object AbstractOverTypesMain extends App {

  def testTotalUptime1() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient1(hosts)
    val service = new UptimeService1(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected) // error
  }

  def testTotalUptime2() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient2(hosts)
    val service = new UptimeService2(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    println(actual == expected)
    assert(actual == expected)
  }

//  testTotalUptime1()
  testTotalUptime2()
}

trait UptimeClient1 {
  def getUptime(hostname: String): Future[Int]
}

class UptimeService1(client: UptimeClient1) {
  def getTotalUptime(hostnames: List[String]): Future[Int] =
    hostnames.traverse(client.getUptime).map(_.sum) }

class TestUptimeClient1(hosts: Map[String, Int]) extends UptimeClient1 {
  def getUptime(hostname: String): Future[Int] = Future.successful(hosts.getOrElse(hostname, 0))
}


// -------------------------------------------------------

class UptimeService2[F[_]: Applicative](client: UptimeClient2[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] = hostnames.traverse(client.getUptime).map(_.sum)

}

trait RealUptimeClient extends UptimeClient2[Future] {
  def getUptime(hostname: String): Future[Int]
}
trait TestUptimeClient extends UptimeClient2[Id] {
  def getUptime(hostname: String): Int
}

trait UptimeClient2[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class TestUptimeClient2(hosts: Map[String, Int]) extends UptimeClient2[Id] {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0) }