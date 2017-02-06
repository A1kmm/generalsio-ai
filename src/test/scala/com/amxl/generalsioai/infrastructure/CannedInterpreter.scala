package com.amxl.generalsioai.infrastructure

import cats.arrow.FunctionK
import org.atnos.eff._, syntax.state._
import cats.data.State
import com.amxl.generalsioai.util.EffUtil.Identity
import scala.collection.immutable.Nil

import scala.language.higherKinds

object CannedInterpreter {
  type CannedResponse[T[_]] = FunctionK[T, Identity]
  case class CannedInterpreter[T[_]](cannedResponses: List[CannedResponse[T]]) {
    private def tryPop(): State[List[CannedResponse[T]], CannedResponse[T]] = for {
      l <- State.get[List[CannedResponse[T]]]
      r <- l match {
        case (r::rest) => for {
          _ <- State.set(rest)
        } yield r
        case Nil => throw new RuntimeException("More effects executed than responses available")
      }
    } yield r

    private def transformOne[X](input: T[X]): State[List[CannedResponse[T]], X] = for {
      response <- tryPop()
    } yield response(input)

    private val transformAll : FunctionK[T, State[List[CannedResponse[T]], ?]] =
      Lambda[FunctionK[T, State[List[CannedResponse[T]], ?]]](transformOne(_))
    def apply[R, U : Member.Aux[T, R, ?], A](eff: Eff[R, A]): Eff[U, A] =
      Interpret.transform(eff, transformAll).evalState(cannedResponses)
  }

  def cannedInterpreter[T[_]](responses: List[CannedResponse[T]]): CannedInterpreter[T] =
    CannedInterpreter(responses)

  def respondWith[T[_], X](f : PartialFunction[T[X], X]): CannedResponse[T] = new FunctionK[T, Identity] {
    override def apply[A](fa: T[A]): Identity[A] = f(fa.asInstanceOf[T[X]]).asInstanceOf[A]
  }
}
