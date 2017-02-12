package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction, State}
import com.amxl.generalsioai.models.State._

object MoveFinder {
  def neighbouringCells(coordinate: Coordinate): List[Coordinate] =
    List(
      Coordinate(coordinate.x - 1, coordinate.y), Coordinate(coordinate.x + 1, coordinate.y),
      Coordinate(coordinate.x, coordinate.y - 1), Coordinate(coordinate.x, coordinate.y + 1)
    )

  def canOccupy(state: PlayerVisibleState, coordinate: Coordinate): Boolean =
    state.board.get(coordinate) match {
      case None => false
      case Some(MountainCell) => false
      case Some(UnknownCell) => false
      case _ => true
    }

  def allPossibleMoves(state: PlayerVisibleState, playingTeam: Team): Seq[ProposedAction] = {
    val sourceCells = state.board.view.collect {
      case (coord, State.OccupiedCellState(team, soldiers, cellType)) if team == playingTeam && soldiers > 1 =>
        coord -> (soldiers > 2)
    }

    DoNothingAction +: sourceCells.flatMap { case (sourceCell, isHalfStrengthOption) =>
      neighbouringCells(sourceCell).filter(canOccupy(state, _)).flatMap { target =>
        if (isHalfStrengthOption)
          Seq(ProposedAttackAction(sourceCell, target, halfStrength = false),
              ProposedAttackAction(sourceCell, target, halfStrength = true))
        else
          Seq(ProposedAttackAction(sourceCell, target, halfStrength = false))
      }
    }.toSeq
  }

  def allCoords(state: PlayerVisibleState): Seq[Coordinate] =
    for (x <- 0 until state.size.x; y <- 0 until state.size.y) yield Coordinate(x, y)
}
