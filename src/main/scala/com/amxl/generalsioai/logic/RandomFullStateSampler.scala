package com.amxl.generalsioai.logic

import cats.TraverseFilter
import cats.instances.list._
import com.amxl.generalsioai.models.State._
import org.atnos.eff._
import org.atnos.eff.async._

import scala.util.Random

object RandomFullStateSampler {
  private type HasAsync[R] = MemberIn[Async, R]
  private def shuffleList[R : HasAsync, A](input: List[A]): Eff[R, List[A]] =
    asyncDelay(Random.shuffle(input))
  private val probabilityCellIsCity = 0.05 // TODO check actual value

  private def samplePromoteToCity[R : HasAsync]: Eff[R, Boolean] =
    asyncDelay(Random.nextDouble()).map(v => v < probabilityCellIsCity)

  def baseTransformation(playerVisibleState: PlayerVisibleState): FullState = ???

  def sample[R : HasAsync](playerState: PlayerVisibleState): Eff[R, FullState] = {
    val unknownByPlayer = playerState.scores.map { case (team, score) =>
      val knownLand = playerState.board.count {
        case (_, OccupiedCellState(theTeam, _, _)) if theTeam == team => true
        case _ => false
      }
      val knownArmy = playerState.board.toList.map {
        case (_, OccupiedCellState(theTeam, army, _)) if theTeam == team => army
        case _ => 0
      }.sum
      val canSeeGeneral = playerState.board.toList.exists {
        case (_, OccupiedCellState(theTeam, army, GeneralCell)) if theTeam == team => true
        case _ => false
      }
      (Score(score.land - knownLand, score.army - knownArmy), canSeeGeneral)
    }

    val unknownCells = playerState.board.filter {
      case (_, UnknownCell) => true
      case _ => false
    }.keys.toList

    for {
      shuffledUnknownCells <- shuffleList[R, Coordinate](unknownCells)
      promoteToCity <- TraverseFilter[List].filterA[Eff[R, ?], Coordinate](shuffledUnknownCells)(
        _ => samplePromoteToCity[R])
    } yield ??? /* toFullState(playerState, shuffledUnknownCells, promoteToCity, unknownByPlayer) */
  }
}
