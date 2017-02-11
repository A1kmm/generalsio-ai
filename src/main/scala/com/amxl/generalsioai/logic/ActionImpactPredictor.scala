package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction}
import com.amxl.generalsioai.models.State._

object ActionImpactPredictor {
  def predictImpactOf(action: ProposedAction, state: PlayerVisibleState, team: Team): PlayerVisibleState = {
    action match {
      case DoNothingAction => state
      case ProposedAttackAction(source, dest, halfStrength) => {
        val sourceCell = state.board(source)
        val destCell = state.board(dest)
        sourceCell match {
          case sourceCellOccupied@OccupiedCellState(_, sourceStrength, _) => {
            val destStrengthModifier = destCell match {
              case OccupiedCellState(destTeam, soldiers, _) if destTeam == team => soldiers
              case OccupiedCellState(_, soldiers, _) => -soldiers
              case EmptyCell(_, strength) => -strength
            }
            val attackStrength = if (halfStrength) sourceStrength / 2 else sourceStrength - 1
            val resultingStrength = attackStrength + destStrengthModifier

            val withSourceUpdated = state.copy(board = state.board.updated(source, sourceCellOccupied.copy(
              soldiers = sourceStrength - attackStrength)))

            val ownershipChange = resultingStrength > 0
            val defeatedOpponentOnCell = resultingStrength >= 0

            val (resultingDestCell, defeatedTeamOption): (PossiblyUnknownCellState, Option[Team]) =
              (destCell, ownershipChange, defeatedOpponentOnCell) match {
                case (EmptyCell(cellType, _), false, _) => EmptyCell(cellType, -resultingStrength) -> None
                case (EmptyCell(cellType, _), true, _) => OccupiedCellState(team, resultingStrength, cellType) -> None
                case (OccupiedCellState(destTeam, _, GeneralCell), false, true) => EmptyCell(NormalCell, 0) ->
                  Some(destTeam)
                case (OccupiedCellState(destTeam, _, cellType), false, true) => EmptyCell(cellType, 0) -> None
                case (OccupiedCellState(destTeam, _, cellType), false, false) =>
                  OccupiedCellState(destTeam, -resultingStrength, cellType) -> None
                case (OccupiedCellState(destTeam, _, GeneralCell), true, _) if destTeam != team =>
                  OccupiedCellState(team, resultingStrength, NormalCell) -> Some(destTeam)
                case (OccupiedCellState(destTeam, _, cellType), true, _) =>
                  OccupiedCellState(team, resultingStrength, cellType) -> None
                case (cell, _, _) => cell -> None
              }

            val withDestUpdated = withSourceUpdated.copy(board =
              withSourceUpdated.board.updated(dest, resultingDestCell))

            defeatedTeamOption.map(defeatedTeam => withDestUpdated.copy(board = withDestUpdated.board.map {
              case (coord, st @ OccupiedCellState(curTeam, _, _)) if curTeam == defeatedTeam =>
                coord -> st.copy(team = team)
              case (coord, st) => coord -> st
            }, scores = withDestUpdated.scores.updated(defeatedTeam, Score(0, 0)))).getOrElse(withDestUpdated)
          }
          case _ => state
        }
      }
    }
  }
}
