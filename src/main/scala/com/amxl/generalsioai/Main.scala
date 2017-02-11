import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import cats.data.Reader
import com.amxl.generalsioai.interpreter.LogInterpreter._
import com.amxl.generalsioai.interpreter.ClockInterpreter._
import com.amxl.generalsioai.wsclient.GeneralsClient._
import com.amxl.generalsioai.logic.GameBot._
import com.amxl.generalsioai.logic.PhaseBasedMoveRanker._
import com.amxl.generalsioai.models.ClientActions.ClientAction
import com.amxl.generalsioai.models.ClockActions.ClockAction
import com.amxl.generalsioai.models.LogActions.LogAction
import com.amxl.generalsioai.util.EffUtil.FailedWithMessageException
import org.atnos.eff.ReaderInterpretation.runReader
import org.atnos.eff._

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

object Main {
  private type FullStack = Fx.fx6[Async, LogAction, ClockAction, Reader[ActorSystem, ?],
    Reader[Materializer, ?], ClientAction]

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Usage: generalsio-ai userid gameid")
    } else {
      val actorSystem: ActorSystem = ActorSystem()
      val actorMaterializer: Materializer = ActorMaterializer()(actorSystem)

      while (true) {
        try {
          val asyncEff: Eff[Fx.fx1[Async], Unit] =
            runLogs(runClock(runReader(actorMaterializer)(
              runReader(actorSystem)(
                runGeneralsClient(
                  runBotInClient[FullStack](args(0), args(1), ai))))))

          Await.result(AsyncFutureInterpreter.create(actorSystem.dispatcher: ExecutionContext).runAsync(asyncEff),
            Duration.Inf)
        } catch {
          case FailedWithMessageException(msg) => println("Restarting after failure: " + msg)
        }
      }
    }
  }
}
