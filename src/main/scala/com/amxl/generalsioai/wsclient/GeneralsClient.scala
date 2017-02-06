package com.amxl.generalsioai.wsclient

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import com.amxl.generalsioai.models.ClientActions.{ClientAction, ReadMessage, WriteMessage}
import org.atnos.eff._
import org.atnos.eff.reader._
import org.atnos.eff.async._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.stream.{Attributes, Materializer, OverflowStrategy, QueueOfferResult}
import akka.stream.scaladsl.{Keep, Sink, SinkQueue, Source, SourceQueue}
import cats.Applicative
import cats.data._
import com.amxl.generalsioai.codecs.OfficialMessageCodec
import com.amxl.generalsioai.models.OfficialMessages.{MessageFromServer, MessageToServer}
import com.amxl.generalsioai.util.EffUtil._

object GeneralsClient {
  private val serverUri = "ws://botws.generals.io/socket.io/?EIO=3&transport=websocket"
  private val sourceBufferSize = 5
  private val sinkBufferSize = 3

  private type HasAsync[R] = MemberIn[Async, R]
  private type HasMaterializer[R] = MemberIn[Reader[Materializer, ?], R]

  private def processStringMessage[R : HasAsync : HasMaterializer](sinkQueue: SinkQueue[Message])(message: String):
    Eff[R, MessageFromServer] = message.splitAt(2) match {
      case ("42", body) => OfficialMessageCodec.parseMessageFromServer(body) match {
        case Left(err) =>
          println(s"""Warning: Got error "${err}" while parsing "${body}"""")
          readOneMessage(sinkQueue)
        case Right(parsedMessage) => Applicative[Eff[R, ?]].pure(parsedMessage)
      }
      case _ => readOneMessage(sinkQueue)
  }

  private def readOneMessage[R : HasAsync : HasMaterializer](sinkQueue: SinkQueue[Message]):
    Eff[R, MessageFromServer] = for {
      messageOption <- liftFuture(sinkQueue.pull())
      messageRaw <- messageOption match {
        case None => failWithMessage("Can't read from generals.io WebSocket")
        case Some(messageRaw) => Applicative[Eff[R, ?]].pure(messageRaw)
      }
      result <- messageRaw match {
        case textMessage : TextMessage => readStringSource(textMessage.textStream).flatMap(
          processStringMessage(sinkQueue))
        case _ => readOneMessage(sinkQueue)
      }
    } yield result

  private def writeOneMessage[R : HasAsync](sourceQueue: SourceQueue[Message], msg: MessageToServer):
    Eff[R, Unit] = for {
      result <- liftFuture(sourceQueue.offer(TextMessage("42" + OfficialMessageCodec.encodeMessageToServer(msg))))
      _ <- result match {
        case QueueOfferResult.Dropped =>
          println(s"Queue dropped outgoing message $msg")
          Applicative[Eff[R, ?]].pure(())
        case QueueOfferResult.QueueClosed =>
          failWithMessage[R, Unit]("WebSocket closed")
        case QueueOfferResult.Enqueued => Applicative[Eff[R, ?]].pure(())
        case QueueOfferResult.Failure(e) => asyncFail(e)
      }
  } yield ()

  def runGeneralsClient[R,
      U : Member.Aux[ClientAction, R, ?]
        : HasAsync
        : MemberIn[Reader[ActorSystem, ?], ?]
        : HasMaterializer,
      A](eff: Eff[R, A]): Eff[U, A] = for {
    system <- ask[U, ActorSystem]
    materializer <- ask[U, Materializer]
    webSocketFlow = Http()(system).webSocketClientFlow(WebSocketRequest(serverUri))
    sourceQueueSource = Source.queue[Message](sourceBufferSize, OverflowStrategy.dropHead)
    sinkQueueSink = Sink.queue[Message]().withAttributes(Attributes.inputBuffer(sinkBufferSize, sinkBufferSize))
    ((sourceQueue, upgradeFuture), sinkQueue) =
      sourceQueueSource.viaMat(webSocketFlow)(Keep.both).toMat(sinkQueueSink)(Keep.both).run()(materializer)
    upgrade <- liftFuture(upgradeFuture)
    _ <- failWithMessageIf(upgrade.response.status != StatusCodes.SwitchingProtocols,
      "Unexpected upgrade response " + upgrade.response)

    result <- Interpret.translate(eff) {
      new Translate[ClientAction, U] {
        override def apply[X](action: ClientAction[X]): Eff[U, X] = action match {
          case ReadMessage() => readOneMessage(sinkQueue).map(_.asInstanceOf[X])
          case WriteMessage(msg) => writeOneMessage(sourceQueue, msg).map(_.asInstanceOf[X])
        }
      }
    }

    _ <- asyncNow(sourceQueue.complete())
    _ <- asyncNow(sinkQueue.cancel())
  } yield result
}
