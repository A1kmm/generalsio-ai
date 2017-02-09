package com.amxl.generalsioai.logic

import java.time.Duration

import org.atnos.eff.MemberIn
import org.atnos.eff._
import com.amxl.generalsioai.models.ClientActions._
import com.amxl.generalsioai.models.LogActions._
import com.amxl.generalsioai.models.ClockActions._
import com.amxl.generalsioai.models.OfficialMessages._
import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction}
import com.amxl.generalsioai.models.State.{Coordinate, OfficialStateArrays, PlayerVisibleState}
import org.atnos.eff.syntax.all._
import org.atnos.eff.state._
import cats.data.State

object GameBot {
  type HasClientAction[R] = MemberIn[ClientAction, R]
  type HasLog[R] = MemberIn[LogAction, R]
  type HasClock[R] = MemberIn[ClockAction, R]

  case class Ai(f: PlayerVisibleState => (ProposedAction, Ai))

  val username = "Bot65443"

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

  private def correctForFogMemory[R : MemberIn[State[Option[PlayerVisibleState], ?], ?]](st: PlayerVisibleState):
    Eff[R, PlayerVisibleState] = for {
    oldState <- get[R, Option[PlayerVisibleState]]
  } yield oldState match {
    case None => st
    case Some(oldStateValue) => GameUpdateApplier.rememberDespiteFog(oldStateValue, st)
  }

  private def invokeAndUpdateAi[R : MemberIn[State[Ai, ?], ?]](st: PlayerVisibleState): Eff[R, ProposedAction] = for {
    currentAi <- get[R, Ai]
    (action, nextAi) = currentAi.f(st)
    _ <- put[R, Ai](nextAi)
  } yield action


  private def gameMainLoop[R
      : HasClientAction
      : HasLog
      : HasClock
      : MemberIn[State[OfficialStateArrays, ?], ?]
      : MemberIn[State[Option[PlayerVisibleState], ?], ?]
      : MemberIn[State[Ai, ?], ?]]: Eff[R, Unit] = for {
    preReadTime <- currentTime[R]()
    msg <- readMessage[R]()
    postReadTime <- currentTime[R]()
    _ <- msg match {
      case GameLost => writeLog("We lost :'(")
      case GameWon => writeLog("We won!!!!!")
      case update: GameUpdate =>
        for {
          oldState <- get[R, OfficialStateArrays]
          newState = GameUpdateApplier.updateOfficialState(update, oldState)
          _ <- put[R, OfficialStateArrays](newState)
          playerVisible <- GameUpdateApplier.officialStateToPlayerVisibleState(newState, update) match {
            case Right(v) => correctForFogMemory(v).map(Right(_))
            case x => Eff.pure[R, Either[String, PlayerVisibleState]](x)
          }
          _ <- playerVisible match {
            case Left(errMsg) => writeLog[R](errMsg)
            case Right(playerVisibleState) if Duration.between(preReadTime, postReadTime).toMillis >= 250 =>
              for {
                proposedAction <- invokeAndUpdateAi(playerVisibleState)
                _ <- doProposedAction[R](proposedAction, playerVisibleState)
                _ <- gameMainLoop[R]
              } yield ()
            case _ => gameMainLoop[R]
          }
        } yield ()

      case _ => for {
        _ <- writeLog("Other message received during game: " + msg)
        _ <- gameMainLoop
      } yield ()
    }
  } yield ()

  private type ExtraStack = Fx3[State[Option[PlayerVisibleState], ?], State[OfficialStateArrays, ?], State[Ai, ?]]

  def runBotInClient[R : HasClientAction : HasLog : HasClock](
         userId: String, gameId: String, ai: Ai):
    Eff[R, Unit] = for {
    _ <- writeMessage[R](SetUsername(userId, username))
    _ <- writeMessage[R](StarsAndRank(userId))
    _ <- writeMessage[R](if (gameId == "1v1") Join1v1(userId) else JoinPrivate(gameId, userId))
    _ <- writeMessage[R](SetForceStart(gameId, isForced = true))
    stateArrays <- waitForGameStart()

    _ <- gameMainLoop[Fx.append[ExtraStack, R]]
          .runState(ai)
          .runState(stateArrays)
          .runState(Option.empty[PlayerVisibleState])

    _ <- writeMessage[R](LeaveGame())
  } yield ()
}
