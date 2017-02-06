package com.amxl.generalsioai.interpreter

import com.amxl.generalsioai.models.LogActions._
import org.atnos.eff._
import org.atnos.eff.async._

object LogInterpreter {
  private type HasAsync[R] = MemberIn[Async, R]

  def runLogs[R, U: Member.Aux[LogAction, R, ?] : HasAsync, A](eff: Eff[R, A]): Eff[U, A] =
    Interpret.translate(eff) {
      new Translate[LogAction, U] {
        override def apply[X](action: LogAction[X]): Eff[U, X] = action match {
          case WriteLog(msg) => asyncDelay(println(msg)).map(_.asInstanceOf[X])
        }
      }
    }
}
