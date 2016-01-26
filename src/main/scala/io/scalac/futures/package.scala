package io.scalac

import shapeless.{HNil, ::}

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

package object futures {

  class FailFastFuture extends scalaz.Applicative[Future] {
    override def point[A](a: => A): Future[A] = Future(a)

    override def ap[A, B](fa: => Future[A])(f: => Future[(A) => B]): Future[B] = {
      val promise = Promise[B]()

      fa.onFailure { case ex => promise.tryFailure(ex) }
      f.onFailure { case ex => promise.tryFailure(ex) }

      (fa zip f).onSuccess { case (v, f) => promise.trySuccess(f(v)) }

      promise.future
    }
  }

  class FutureInstance(implicit ec: ExecutionContext) extends scalaz.Monad[Future] {
    override def point[A](a: => A): Future[A] = Future(a)

    override def bind[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa flatMap f
  }

}

object FfRunner extends App {

  import shapeless._
  import shapeless.contrib.scalaz._

  import scalaz.Applicative
  import scalaz.std.option._


  implicit val ff = new futures.FailFastFuture

  val key = Future {
    Thread.sleep(5000)
    1
  }

  val value = Future {
    //Thread.sleep(1000)
    throw new Exception("Failure")
    "a"
  }

  val r = shapeless.contrib.scalaz.sequence(key :: value :: HNil).map {
    case a :: b :: HNil => s"kv: $a - $b"
  }

  import scala.concurrent.duration._
  println(Await.result(r, 7 second))
}