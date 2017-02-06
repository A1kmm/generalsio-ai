package com.amxl.generalsioai.logic

import org.specs2.mutable.Specification
import GameUpdateApplier._
import com.amxl.generalsioai.models.OfficialMessages.{GameUpdate, ScoreDetails}
import com.amxl.generalsioai.models.State._

class GameUpdateApplierSpec extends Specification {
  private val blankUpdate = GameUpdate(scores = List(), turn = 0, generals = List(),
    mapDiff = List(), citiesDiff = List())
  "Should apply generals" >> {
    updateOfficialState(blankUpdate.copy(generals = List(1, 2, 3)), initialOfficialState(0)).generals must
      be_==(List(1, 2, 3))
  }

  "Should set up initial map state" >> {
    updateOfficialState(blankUpdate.copy(mapDiff = List(0, 5, 3, 5, 7, 11, 13)), initialOfficialState(0)).map must
        be_==(List(3, 5, 7, 11, 13))
  }

  "Should update map state" >> {
    updateOfficialState(
      blankUpdate.copy(mapDiff = List(3, 1, 12, 1)),
      initialOfficialState(0).copy(map = List(3, 5, 7, 11, 13))).map must be_==(List(3, 5, 7, 12, 13))
  }

  "Should set up initial cities state" >> {
    updateOfficialState(blankUpdate.copy(citiesDiff = List(0, 5, 3, 5, 7, 11, 13)), initialOfficialState(0)).cities must
        be_==(List(3, 5, 7, 11, 13))
  }

  "Should update cities state" >> {
    updateOfficialState(
      blankUpdate.copy(citiesDiff = List(3, 1, 12, 1)),
      initialOfficialState(0).copy(cities = List(3, 5, 7, 11, 13))).cities must be_==(List(3, 5, 7, 12, 13))
  }

  "Should convert official state to player visible state" >> {
    val result = officialStateToPlayerVisibleState(
      OfficialStateArrays(
        attackIndex = 1,
        generals = List(3, 14),
        map = List(
          4, 5,
          // Counts...
          0, 0,  1, 25,
          0, 0,  0,  1,
          0,15,  0,  0,
          0, 0, 25,  0,
          0, 0,  0,  0,
          // Terrain...
          -4, -1,  0, 0,
          -2, -3, -1, 0,
          -3, -1, -1, -1,
          -3, -1,  1, -1,
          -1, -1, -1, -1
        ),
        cities = List(2, 9)
      ),
      GameUpdate(
        scores = List(
          ScoreDetails(total = 27, tiles = 3, i = 0, dead = false),
          ScoreDetails(total = 25, tiles = 1, i = 1, dead = false),
          ScoreDetails(total = 0, tiles = 0, i = 2, dead = true)
        ),
        turn = 0,
        generals = List(),
        mapDiff = List(),
        citiesDiff = List()
      )
    )

    result must be_==(Right(PlayerVisibleState(
      size = Coordinate(4, 5),
      scores = Map(Team(0) -> Score(land = 3, army = 27),
                   Team(1) -> Score(land = 1, army = 25)),
      board = Map(
        Coordinate(0,0) -> MountainCell, Coordinate(1,0) -> EmptyCell(NormalCell, 0), Coordinate(2,0) -> OccupiedCellState(Team(0), 1, CityCell), Coordinate(3,0) -> OccupiedCellState(Team(0), 25, GeneralCell),
        Coordinate(0,1) -> MountainCell, Coordinate(1,1) -> UnknownCell, Coordinate(2,1) -> EmptyCell(NormalCell, 0), Coordinate(3,1) -> OccupiedCellState(Team(0), 1, NormalCell),
        Coordinate(0,2) -> UnknownCell, Coordinate(1,2) -> EmptyCell(CityCell, 15), Coordinate(2,2) -> EmptyCell(NormalCell, 0), Coordinate(3,2) -> EmptyCell(NormalCell, 0),
        Coordinate(0,3) -> UnknownCell, Coordinate(1,3) -> EmptyCell(NormalCell, 0), Coordinate(2,3) -> OccupiedCellState(Team(1), 25, GeneralCell), Coordinate(3,3) -> EmptyCell(NormalCell, 0),
        Coordinate(0,4) -> EmptyCell(NormalCell, 0), Coordinate(1,4) -> EmptyCell(NormalCell, 0), Coordinate(2,4) -> EmptyCell(NormalCell, 0), Coordinate(3,4) -> EmptyCell(NormalCell, 0)
      ),
      playingAsTeam = Team(1),
      turn = 0,
      turnsUntilLandBonus = 25
    )))
  }
}
