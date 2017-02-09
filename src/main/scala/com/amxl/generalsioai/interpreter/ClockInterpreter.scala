package com.amxl.generalsioai.interpreter

import com.amxl.generalsioai.models.ClockActions.{ClockAction, CurrentTime}
import org.atnos.eff._
import org.atnos.eff.async._
import java.time.Instant

object ClockInterpreter {
  private type HasAsync[R] = MemberIn[Async, R]

  def runClock[R, U: Member.Aux[ClockAction, R, ?] : HasAsync, A](eff: Eff[R, A]): Eff[U, A] =
    Interpret.translate(eff) {
      new Translate[ClockAction, U] {
        override def apply[X](action: ClockAction[X]): Eff[U, X] = action match {
          case CurrentTime() => asyncNow(Instant.now()).map(_.asInstanceOf[X])
        }
      }
    }
}
