package com.amxl.generalsioai.logic

import org.atnos.eff.MemberIn
import org.atnos.eff._
import com.amxl.generalsioai.models.ClientActions._
import com.amxl.generalsioai.models.LogActions._
import com.amxl.generalsioai.models.OfficialMessages._
import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction}
import com.amxl.generalsioai.models.State.{Coordinate, OfficialStateArrays, PlayerVisibleState}
import org.atnos.eff.syntax.all._
import org.atnos.eff.state._
import cats.data.State

object GameBot {
  type HasClientAction[R] = MemberIn[ClientAction, R]
  type HasLog[R] = MemberIn[LogAction, R]

  val userId = "B1XnKMISe"
  val username = "DefeatinatorBotv1"

  private def waitForGameStart[R : HasClientAction : HasLog](): Eff[R, OfficialStateArrays] =
    readMessage().flatMap {
      case GameStart(attackIndex, replayId, _, _) =>
        writeLog[R](s"Replay ID: $replayId").map(_ =>
          GameUpdateApplier.initialOfficialState(attackIndex))
      case msg => for {
        _ <- writeLog(s"Skipping message while waiting for start: $msg")
        r <- waitForGameStart()
      } yield (r)
    }

  private def doProposedAction[R : HasClientAction : HasLog](action: ProposedAction, state: PlayerVisibleState):
      Eff[R, Unit] = {
    def coordToIdx(c: Coordinate) = c.y * state.size.x + c.x
    action match {
      case DoNothingAction =>
        writeLog("The best action is inaction")
      case ProposedAttackAction(source, dest, halfStrength) => for {
        _ <- writeMessage(Attack(source = coordToIdx(source), dest = coordToIdx(dest), halfStrength))
        _ <- writeLog(s"Attacking from $source to $dest - ${if (halfStrength) "half" else "full"} strength")
      } yield ()
    }
  }

  private def gameMainLoop[R : HasClientAction : HasLog : MemberIn[State[OfficialStateArrays, ?], ?]](
    ai: PlayerVisibleState => ProposedAction): Eff[R, Unit] = {
    println("In gameMainLoop, about to readMessage...")
    readMessage().flatMap {
      case GameLost => writeLog("We lost :'(")
      case GameWon => writeLog("We won!!!!!")
      case update: GameUpdate => {
        println("Processing update " + update)
        for {
          oldState <- get[R, OfficialStateArrays]
          newState = GameUpdateApplier.updateOfficialState(update, oldState)
          _ <- put[R, OfficialStateArrays](newState)
          _ <- GameUpdateApplier.officialStateToPlayerVisibleState(newState, update) match {
            case Right(playerVisibleState) =>
              val proposedAction = ai(playerVisibleState)
              for {
                _ <- doProposedAction(proposedAction, playerVisibleState)
                _ <- gameMainLoop[R](ai)
              } yield ()
            case Left(msg) => writeLog(msg)
          }
        } yield ()
      }

      case msg => for {
        _ <- writeLog("Other message received during game: " + msg)
        _ <- gameMainLoop(ai)
      } yield ()
    }
  }


  def runBotInClient[R : HasClientAction : HasLog](gameId: String, ai: PlayerVisibleState => ProposedAction):
    Eff[R, Unit] = for {
    _ <- writeMessage[R](StarsAndRank(userId))
    _ <- writeMessage[R](JoinPrivate(gameId, userId))
    _ <- writeMessage[R](SetUsername(userId, username))
    _ <- writeMessage[R](SetForceStart(gameId, isForced = true))
    stateArrays <- waitForGameStart()

    _ <- gameMainLoop[Fx.prepend[State[OfficialStateArrays, ?], R]](ai).runState(stateArrays)

    _ <- writeMessage[R](LeaveGame())
  } yield ()
}
