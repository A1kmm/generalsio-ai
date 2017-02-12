package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction}
import com.amxl.generalsioai.models.State._

object ActionImpactPredictor {
  def updateForLand(state: PlayerVisibleState): PlayerVisibleState = {
    val (generalLandBonus, newTurnsForBonus) =
      if (state.turnsUntilLandBonus == 0)
        (1, 25)
      else
        (0, state.turnsUntilLandBonus - 1)
    state.copy(turnsUntilLandBonus = newTurnsForBonus, board = state.board.mapValues {
      case s@OccupiedCellState(_, soldiers, GeneralCell) => s.copy(soldiers = soldiers + 1)
      case s@OccupiedCellState(_, soldiers, CityCell) => s.copy(soldiers = soldiers + 1)
      case s@OccupiedCellState(_, soldiers, _) => s.copy(soldiers = soldiers + generalLandBonus)
      case cellState => cellState
    })
  }

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
              case _ => -1
            }
            val attackStrength = if (halfStrength) sourceStrength / 2 else sourceStrength - 1
            val resultingStrength = attackStrength + destStrengthModifier

            val withSourceUpdated = state.copy(board = state.board.updated(source, sourceCellOccupied.copy(
              soldiers = sourceStrength - attackStrength)))

            val ownershipChange = resultingStrength > 0

            val (resultingDestCell, defeatedTeamOption): (PossiblyUnknownCellState, Option[Team]) =
              (destCell, ownershipChange) match {
                case (EmptyCell(cellType, _), false) => EmptyCell(cellType, -resultingStrength) -> None
                case (EmptyCell(cellType, _), true) => OccupiedCellState(team, resultingStrength, cellType) -> None
                case (OccupiedCellState(destTeam, _, cellType), false) =>
                  OccupiedCellState(destTeam, -resultingStrength, cellType) -> None
                case (OccupiedCellState(destTeam, _, GeneralCell), true) if destTeam != team =>
                  OccupiedCellState(team, resultingStrength, NormalCell) -> Some(destTeam)
                case (OccupiedCellState(destTeam, _, cellType), true) =>
                  OccupiedCellState(team, resultingStrength, cellType) -> None
                case (cell, _) => cell -> None
              }

            val withDestUpdated = withSourceUpdated.copy(board =
              withSourceUpdated.board.updated(dest, resultingDestCell))

            defeatedTeamOption.map(defeatedTeam => withDestUpdated.copy(board = withDestUpdated.board.mapValues {
              case st @ OccupiedCellState(curTeam, _, _) if curTeam == defeatedTeam =>
                st.copy(team = team)
              case st => st
            }, scores = withDestUpdated.scores.updated(defeatedTeam, Score(0, 0)))).getOrElse(withDestUpdated)
          }
          case _ => state
        }
      }
    }
  }
}
