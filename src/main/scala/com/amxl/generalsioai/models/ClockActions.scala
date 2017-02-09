package com.amxl.generalsioai.models

import org.atnos.eff.{Eff, MemberIn}
import java.time.Instant

object ClockActions {
  sealed trait ClockAction[A]
  case class CurrentTime() extends ClockAction[Instant]

  def currentTime[R : MemberIn[ClockAction, ?]](): Eff[R, Instant] =
    Eff.send(CurrentTime())

}
