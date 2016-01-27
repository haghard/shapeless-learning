package io.scalac

import shapeless.{HNil, ::}

import scala.concurrent.{ Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

package object futures {

  class FailFastFutureApplicative extends scalaz.Applicative[Future] {
    override def point[A](a: => A): Future[A] = Future(a)

    override def ap[A, B](fa: => Future[A])(f: => Future[(A) => B]): Future[B] = {
      val promise = Promise[B]()

      fa.onFailure { case ex => promise.tryFailure(ex) }
      f.onFailure { case ex => promise.tryFailure(ex) }

      (fa zip f).onSuccess { case (v, f) => promise.trySuccess(f(v)) }

      promise.future
    }
  }
}

object FfRunner extends App {
  import futures._
  import shapeless._
  import shapeless.contrib.scalaz._

  import scalaz.Applicative
  import scalaz.std.option._

  /*
  implicit def monadInstanses[A](implicit ex: ExecutionContext) =
    new Monad[({type f[a] = FailableFuture[Exception, a]})#f] {

      override def point[A](a: => A): FailableFuture[Exception, A] =
        FailableFuture { Future { a.successNel[Exception] } }

      override def bind[A, B](fa: FailableFuture[Exception, A])
                             (f: (A) => FailableFuture[Exception, B]): FailableFuture[Exception, B] = {
        fa.flatMap[B](f)
      }

    }
  */

  implicit val ff = new futures.FailFastFutureApplicative

  val key = Future {
    println("run  key")
    Thread.sleep(5000)
    1
  }

  val value = Future {
    println("run  value")
    Thread.sleep(1000)
    throw new Exception("Failure during value calculation")
  }


  val r = scalaz.Applicative[Future].apply2(key, value) { case (a,b) => s"kv: $a - $b" }

  /*val r = shapeless.contrib.scalaz.sequence(key :: value :: HNil).map {
    case a :: b :: HNil => s"kv: $a - $b"
  }*/

  import scala.concurrent.duration._
  println(Await.result(r, 7 second))
}