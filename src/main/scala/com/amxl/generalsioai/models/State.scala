package com.amxl.generalsioai.models

object State {
  sealed trait OccupiableCellType
  case object NormalCell extends OccupiableCellType
  case object GeneralCell extends OccupiableCellType
  case object CityCell extends OccupiableCellType

  sealed trait KnownCellState
  sealed trait PossiblyUnknownCellState

  case class OccupiedCellState(team: Team, soldiers: Int, cellType: OccupiableCellType)
    extends KnownCellState with PossiblyUnknownCellState
  case class EmptyCell(cellType: OccupiableCellType) extends KnownCellState with PossiblyUnknownCellState
  case object MountainCell extends KnownCellState with PossiblyUnknownCellState
  case object UnknownCell extends PossiblyUnknownCellState

  case class Coordinate(x: Int, y: Int)
  case class Team(teamId: Int)

  case class Score(land: Int, army: Int)

  case class FullState(size: Coordinate, board: Map[Coordinate, KnownCellState])
  case class PlayerVisibleState(size: Coordinate, scores: Map[Team, Score],
                          board: Map[Coordinate, PossiblyUnknownCellState],
                          playingAsTeam: Team, turnsUntilLandBonus: Int)

  case class OfficialStateArrays(generals: List[Int], map: List[Int], cities: List[Int])
}
