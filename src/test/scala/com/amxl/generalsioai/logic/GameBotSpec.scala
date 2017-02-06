package com.amxl.generalsioai.logic

import com.amxl.generalsioai.infrastructure.CannedInterpreter.{respondWith, _}
import com.amxl.generalsioai.models.ProposedAttackAction
import com.amxl.generalsioai.models.State.Coordinate
import org.specs2.mutable.Specification
import com.amxl.generalsioai.models.ClientActions.{ClientAction, ReadMessage, WriteMessage}
import com.amxl.generalsioai.models.LogActions.{LogAction, WriteLog}
import com.amxl.generalsioai.models.OfficialMessages._
import org.atnos.eff._
import org.atnos.eff.syntax.all._

class GameBotSpec extends Specification {
  "Should do the commands requested by AI" >> {
    val mockGameInterpreter = cannedInterpreter[ClientAction](List(
      respondWith[ClientAction, Unit] { case WriteMessage(StarsAndRank(GameBot.userId)) => () },
      respondWith[ClientAction, Unit] { case WriteMessage(JoinPrivate("mygameid", GameBot.userId)) => () },
      respondWith[ClientAction, Unit] { case WriteMessage(SetUsername(GameBot.userId, GameBot.username)) => () },
      respondWith[ClientAction, Unit] { case WriteMessage(SetForceStart(GameBot.userId, true)) => () },
      respondWith[ClientAction, MessageFromServer] { case ReadMessage() => GameStart(1, "replay", "chat", List()) },
      respondWith[ClientAction, MessageFromServer] { case ReadMessage() => GameUpdate(
        scores = List(ScoreDetails(1, 1, 1, dead = false)), turn = 0, generals = List(0),
        mapDiff = List(0, 4, 1, 1, 1, 1), citiesDiff = List(0, 0)) },
      respondWith[ClientAction, Unit] { case WriteMessage(Attack(0, 1, false)) => () },
      respondWith[ClientAction, MessageFromServer] { case ReadMessage() => GameWon },
      respondWith[ClientAction, Unit] { case WriteMessage(LeaveGame()) => () }
    ))
    val mockLogInterpreter = cannedInterpreter[LogAction](List(
      respondWith[LogAction, Unit] { case WriteLog("Replay ID: replay") => () },
      respondWith[LogAction, Unit] { case WriteLog("Attacking from Coordinate(0,0) to Coordinate(0,1) - full strength") => () },
      respondWith[LogAction, Unit] { case WriteLog("We won!!!!!") => ()}
    ))

    val result = mockLogInterpreter(mockGameInterpreter(GameBot.runBotInClient[Fx2[LogAction, ClientAction]]("mygameid",
      _ => ProposedAttackAction(Coordinate(0, 0), Coordinate(0, 1), halfStrength = false)))).run

    result should be_==(())
  }
}
