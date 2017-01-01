package com.amxl.generalsioai.models
import org.atnos.eff._

object ClientActions {
  sealed trait ClientAction[A]
  case class ReadMessage() extends ClientAction[OfficialMessages.MessageFromServer]
  case class WriteMessage(msg: OfficialMessages.MessageToServer) extends ClientAction[Unit]

  def readMessage[R : MemberIn[ClientAction, ?]](): Eff[R, OfficialMessages.MessageFromServer] =
    Eff.send(ReadMessage())
  def writeMessage[R : MemberIn[ClientAction, ?]](msg : OfficialMessages.MessageToServer): Eff[R, Unit] =
    Eff.send(WriteMessage(msg))
}
