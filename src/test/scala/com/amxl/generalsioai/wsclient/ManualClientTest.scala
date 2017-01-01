package com.amxl.generalsioai.wsclient

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import cats.data.Reader
import org.atnos.eff._
import ReaderInterpretation._
import GeneralsClient._
import com.amxl.generalsioai.models.ClientActions._
import com.amxl.generalsioai.models.OfficialMessages._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

import com.amxl.generalsioai.util.EffUtil._

object ManualClientTest {
  private val userId = "B1XnKMISe"
  private type FullStack = Fx.fx4[Async, Reader[ActorSystem, ?], Reader[Materializer, ?], ClientAction]
  def monadicMain(gameId: String) : Eff[FullStack, Unit] = for {
    _ <- writeMessage[FullStack](StarsAndRank(userId))
    _ <- writeMessage[FullStack](JoinPrivate(gameId, userId))
    _ <- writeMessage[FullStack](SetUsername(userId, "MyTest123"))
    msg <- readMessage[FullStack]()
    _ <- writeln[FullStack](msg.toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: ManualClientTest gameId")
      System.exit(1)
    }

    val actorSystem : ActorSystem = ActorSystem()
    val actorMaterializer : Materializer = ActorMaterializer()(actorSystem)

    val asyncEff : Eff[Fx.fx1[Async], Unit] =
        runReader(actorMaterializer)(runReader(actorSystem)(runGeneralsClient(monadicMain(args(0)))))

    Await.result(AsyncFutureInterpreter.create(actorSystem.dispatcher : ExecutionContext).runAsync(asyncEff),
      Duration.Inf)

    actorSystem.terminate()
  }
}
