package com.amxl.generalsioai.models

object State {
  sealed trait OccupiedCellType
  case object NormalCell extends OccupiedCellType
  case object GeneralCell extends OccupiedCellType
  case object CityCell extends OccupiedCellType

  sealed trait KnownCellState
  sealed trait PossiblyUnknownCellState

  case class OccupiedCellState(team: Team, soldiers: Int, cellType: OccupiedCellType)
    extends KnownCellState with PossiblyUnknownCellState
  case object MountainCell extends KnownCellState with PossiblyUnknownCellState
  case object UnknownCell extends PossiblyUnknownCellState

  case class Coordinate(x: Int, y: Int)
  case class Team(teamId: Int)

  case class Score(land: Int, army: Int)

  case class FullState(size: Coordinate, board: Map[Coordinate, KnownCellState])
  case class PartialState(size: Coordinate, scores: Map[Team, Score], board: Map[Coordinate, PossiblyUnknownCellState])

  case class OfficialStateArrays(generals: List[Int], map: List[Int], cities: List[Int])
}
