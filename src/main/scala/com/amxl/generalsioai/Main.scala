import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import cats.data.Reader
import com.amxl.generalsioai.interpreter.LogInterpreter._
import com.amxl.generalsioai.wsclient.GeneralsClient._
import com.amxl.generalsioai.logic.GameBot._
import com.amxl.generalsioai.logic.MultiObjectiveMoveRanker._
import com.amxl.generalsioai.models.ClientActions.ClientAction
import com.amxl.generalsioai.models.LogActions.LogAction
import org.atnos.eff.ReaderInterpretation.runReader
import org.atnos.eff._

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

object Main {
  private type FullStack = Fx.fx5[Async, LogAction, Reader[ActorSystem, ?], Reader[Materializer, ?], ClientAction]

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: generalsio-ai gameid")
    } else {
      val actorSystem: ActorSystem = ActorSystem()
      val actorMaterializer: Materializer = ActorMaterializer()(actorSystem)

      val asyncEff: Eff[Fx.fx1[Async], Unit] =
        runLogs(runReader(actorMaterializer)(
          runReader(actorSystem)(
            runGeneralsClient(
              runBotInClient[FullStack](args(0), pickBestMove)))))

      Await.result(AsyncFutureInterpreter.create(actorSystem.dispatcher: ExecutionContext).runAsync(asyncEff),
        Duration.Inf)

      actorSystem.terminate()
    }
  }
}
