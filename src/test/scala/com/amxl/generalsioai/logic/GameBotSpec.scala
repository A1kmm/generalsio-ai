package com.amxl.generalsioai.logic

import java.time.Instant

import com.amxl.generalsioai.infrastructure.CannedInterpreter.{respondWith, _}
import com.amxl.generalsioai.models.ProposedAttackAction
import com.amxl.generalsioai.models.State.Coordinate
import org.specs2.mutable.Specification
import com.amxl.generalsioai.models.ClientActions.{ClientAction, ReadMessage, WriteMessage}
import com.amxl.generalsioai.models.ClockActions._
import com.amxl.generalsioai.models.LogActions.{LogAction, WriteLog}
import com.amxl.generalsioai.models.OfficialMessages._
import org.atnos.eff._
import org.atnos.eff.syntax.all._

class GameBotSpec extends Specification {
  "Should do the commands requested by AI" >> {
    val mockGameInterpreter = cannedInterpreter[ClientAction]("Client actions", List(
      respondWith[ClientAction, Unit] { case WriteMessage(SetUsername("myuserid", GameBot.username)) => () },
      respondWith[ClientAction, Unit] { case WriteMessage(StarsAndRank("myuserid")) => () },
      respondWith[ClientAction, Unit] { case WriteMessage(JoinPrivate("mygameid", "myuserid")) => () },
      respondWith[ClientAction, Unit] { case WriteMessage(SetForceStart("mygameid", true)) => () },
      respondWith[ClientAction, MessageFromServer] { case ReadMessage() => GameStart(1, "replay", "chat", List()) },
      respondWith[ClientAction, MessageFromServer] { case ReadMessage() => GameUpdate(
        scores = List(ScoreDetails(1, 1, 1, dead = false)), turn = 0, generals = List(0),
        mapDiff = List(0, 4, 1, 1, 1, 1), citiesDiff = List(0, 0)) },
      respondWith[ClientAction, Unit] { case WriteMessage(Attack(0, 1, false)) => () },
      respondWith[ClientAction, MessageFromServer] { case ReadMessage() => GameWon },
      respondWith[ClientAction, Unit] { case WriteMessage(LeaveGame()) => () }
    ))
    val mockLogInterpreter = cannedInterpreter[LogAction]("Logs", List(
      respondWith[LogAction, Unit] { case WriteLog("Replay ID: replay") => () },
      respondWith[LogAction, Unit] { case WriteLog("Attacking from Coordinate(0,0) to Coordinate(0,1) - full strength") => () },
      respondWith[LogAction, Unit] { case WriteLog("We won!!!!!") => ()}
    ))
    val mockClockInterpreter = cannedInterpreter[ClockAction]("Clock", List(
      respondWith[ClockAction, Instant] { case CurrentTime() => Instant.parse("2017-02-01T00:00:00.00Z") },
      respondWith[ClockAction, Instant] { case CurrentTime() => Instant.parse("2017-02-01T00:00:00.50Z") },
      respondWith[ClockAction, Instant] { case CurrentTime() => Instant.parse("2017-02-01T00:00:02.00Z") },
      respondWith[ClockAction, Instant] { case CurrentTime() => Instant.parse("2017-02-01T00:00:02.50Z") }
    ))

    def testAi : GameBot.Ai = GameBot.Ai(_ => (ProposedAttackAction(Coordinate(0, 0), Coordinate(0, 1),
                                                                    halfStrength = false),
                                         testAi))

    val result = mockLogInterpreter(mockClockInterpreter(
      mockGameInterpreter(GameBot.runBotInClient[Fx3[LogAction, ClockAction, ClientAction]]("myuserid", "mygameid",
        testAi)))).run

    result should be_==(())
  }
}
