package com.amxl.generalsioai.logic

import cats._
import cats.instances.all._
import com.amxl.generalsioai.models.State._
import com.amxl.generalsioai.models.OfficialMessages._
import com.amxl.generalsioai.util.EffUtil._

object GameUpdateApplier {
  private def applyDiff(diff: List[Int], orig: List[Int]): List[Int] = {
    diff match {
      case Nil => Nil
      case straightCopy::Nil => orig.take(straightCopy)
      case straightCopy::length::Nil => orig.take(straightCopy)
      case straightCopy::length::rest =>
        val (restData, diffTail) = rest.splitAt(length)
        val (origData, origMid) = orig.splitAt(straightCopy)
        val origTail = origMid.drop(length)
        origData ++ restData ++ applyDiff(diffTail, origTail)
    }
  }

  def initialOfficialState(attackIndex: Int) = OfficialStateArrays(generals = List(), map = List(), cities = List(),
    attackIndex = attackIndex)

  def updateOfficialState(update: GameUpdate, state: OfficialStateArrays): OfficialStateArrays =
    state.copy(generals = update.generals, map = applyDiff(update.mapDiff, state.map),
      cities = applyDiff(update.citiesDiff, state.cities))

  type GameUpdateResult[A] = Either[String, A]

  def officialStateToPlayerVisibleState(stateArrays: OfficialStateArrays, lastUpdate: GameUpdate):
    Either[String, PlayerVisibleState] = {
    stateArrays.map match {
      case width::height::allData if allData.length == width * height * 2 =>
        for {
          _ <- when[GameUpdateResult](width <= 0)(Left("Width must be positive"))
          _ <- when[GameUpdateResult](height <= 0)(Left("Height must be positive"))
          (armyCounts, terrainTypes) = allData.splitAt(width * height)
          scores = Map(lastUpdate.scores.filter(scoreDetails => !scoreDetails.dead)
                                        .map(scoreDetails => Team(scoreDetails.i) -> Score(land = scoreDetails.tiles,
                                             army = scoreDetails.total)) :_ *)
          _ <- when[GameUpdateResult](lastUpdate.turn < 0)(Left("Turn must not be negative"))
          turnMod25 = (lastUpdate.turn / 2) % 50
          coords = (0 until height).flatMap(y => (0 until width).map(x => Coordinate(x, y))).toList
          idxToCoord = (idx: Int) => Coordinate(idx % width, idx / width)
          cellTypeMap = Map(stateArrays.generals.map(idx => idxToCoord(idx) -> GeneralCell) ++
                            stateArrays.cities.map(idx => idxToCoord(idx) -> CityCell) : _*)
          boardAsList <- Traverse[List].traverse(coords.zip(armyCounts.zip(terrainTypes))) {
            case (coord, (_, terrainType)) if terrainType == -3 => Right(coord -> UnknownCell)
            case (coord, (_, terrainType)) if terrainType == -4 || terrainType == -2 => Right(coord -> MountainCell)
            case (coord, (armyCount, terrainType)) if terrainType == -1 => Right(coord -> EmptyCell(
              cellType = cellTypeMap.getOrElse(coord, NormalCell), strength = armyCount))
            case (coord, (armyCount, terrainType)) if scores.contains(Team(terrainType)) =>
              Right(coord -> OccupiedCellState(team = Team(terrainType), soldiers = armyCount,
                cellType = cellTypeMap.getOrElse(coord, NormalCell)))
            case (_, (_, terrainType)) => Left(s"Unknown terrain type $terrainType encountered")
          }
        } yield
          PlayerVisibleState(
            size = Coordinate(width, height),
            scores = scores,
            board = Map(boardAsList :_*),
            playingAsTeam = Team(stateArrays.attackIndex),
            turn = lastUpdate.turn,
            turnsUntilLandBonus = if (turnMod25 == 0) 25 else turnMod25
          )

      case _ => Left("Map array had wrong length: " + stateArrays.map)
    }
  }

  def rememberDespiteFog(oldState: PlayerVisibleState, currentState: PlayerVisibleState): PlayerVisibleState = {
    currentState.copy(board = currentState.board.map {
      case (coord, UnknownCell) =>
        oldState.board(coord) match {
          case t : EmptyCell => coord -> t
          case OccupiedCellState(team, soldiers, cellType) => coord -> OccupiedCellState(Team(-1), soldiers, cellType)
          case _ => coord -> UnknownCell
        }
      case (coord, state) => coord -> state
    })
  }

}
