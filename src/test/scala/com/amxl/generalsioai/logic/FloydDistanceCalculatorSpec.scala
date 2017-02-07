package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.State._
import org.specs2.mutable.Specification

class FloydDistanceCalculatorSpec extends Specification {
  "Distance calculator" >> {
    val dists = FloydDistanceCalculator.distanceMapForPlayerState(PlayerVisibleState(
      size = Coordinate(3, 3), scores = Map(),
      board = Map(
        Coordinate(0, 0) -> EmptyCell(NormalCell, 0), Coordinate(1, 0) -> EmptyCell(NormalCell, 0), Coordinate(2, 0) -> EmptyCell(NormalCell, 0),
        Coordinate(0, 1) -> MountainCell, Coordinate(1, 1) -> EmptyCell(NormalCell, 0), Coordinate(2, 1) -> MountainCell,
        Coordinate(0, 2) -> EmptyCell(NormalCell, 0), Coordinate(1, 2) -> EmptyCell(NormalCell, 0), Coordinate(2, 2) -> EmptyCell(NormalCell, 2)
      ),
      playingAsTeam = Team(0), turn = 0, turnsUntilLandBonus = 50
    ))

    dists(Coordinate(0, 0))(Coordinate(0, 0)) should be_==(Some(0))
    dists(Coordinate(0, 0))(Coordinate(1, 0)) should be_==(Some(1))
    dists(Coordinate(0, 0))(Coordinate(2, 0)) should be_==(Some(2))

    dists(Coordinate(0, 0))(Coordinate(0, 1)) should be_==(None)
    dists(Coordinate(0, 0))(Coordinate(1, 1)) should be_==(Some(2))
    dists(Coordinate(0, 0))(Coordinate(2, 1)) should be_==(None)

    dists(Coordinate(0, 0))(Coordinate(0, 2)) should be_==(Some(4))
    dists(Coordinate(0, 0))(Coordinate(1, 2)) should be_==(Some(3))
    dists(Coordinate(0, 0))(Coordinate(2, 2)) should be_==(Some(4))
  }
}
