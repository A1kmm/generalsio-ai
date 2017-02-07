package com.amxl.generalsioai.util

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.data._
import org.atnos.eff.{Async, Eff, MemberIn}
import org.atnos.eff.async.{asyncDelay, asyncFail}
import org.atnos.eff.reader._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.language.higherKinds

object EffUtil {
  type Identity[A] = A

  case class FailedWithMessageException(msg: String) extends Exception(msg)

  // This could be improved, but it would require writing a different async interpreter and effects.
  def liftFuture[A, R : MemberIn[Async, ?]](future: Future[A]): Eff[R, A] =
    asyncDelay(Await.result(future, Duration.Inf))

  def when[F[_] : Applicative](cond: Boolean)(onTrue: => F[Unit]): F[Unit] =
    if (cond) onTrue else Applicative[F].pure(())

  def failWithMessage[R : MemberIn[Async, ?], A](msg: String): Eff[R, A] =
    asyncFail[R, A](new FailedWithMessageException(msg))

  def failWithMessageIf[R : MemberIn[Async, ?]](cond: Boolean, msg: String): Eff[R, Unit] =
    when[Eff[R, ?]](cond)(failWithMessage[R, Unit](msg))

  def readStringSource[R : MemberIn[Async, ?] : MemberIn[Reader[Materializer, ?], ?], A](
       source: Source[String, A]): Eff[R, String] = for {
    materializer <- ask[R, Materializer]
    r <- liftFuture(source.runFold("")(_ + _)(materializer))
  } yield r

  def writeln[R : MemberIn[Async, ?]](msg: String): Eff[R, Unit] = asyncDelay(println(msg))
}
