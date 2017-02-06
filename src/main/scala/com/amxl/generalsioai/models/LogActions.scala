package com.amxl.generalsioai.models

import org.atnos.eff.{Eff, MemberIn}

object LogActions {
  sealed trait LogAction[A]
  case class WriteLog(msg: String) extends LogAction[Unit]

  def writeLog[R : MemberIn[LogAction, ?]](msg: String): Eff[R, Unit] =
    Eff.send(WriteLog(msg))

}
