package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.{ProposedStrategy, _}
import com.amxl.generalsioai.models.State._
import FloydDistanceCalculator.DistanceMap

object MultiObjectiveMoveRanker {
  private val maximumNotProtectiveConcurrentStrategies = 2

  private def allPossibleStrategies(state: PlayerVisibleState): List[ProposedStrategy] =
    state.board.toList.collect {
      case (p, OccupiedCellState(team, _, GeneralCell)) if team != state.playingAsTeam =>
        AttackEnemyGeneral(p)
      case (p, OccupiedCellState(team, _, CityCell)) if team != state.playingAsTeam =>
        TakeCityAt(p)
      case (p, EmptyCell(CityCell, _)) => TakeCityAt(p)
    }

  private def decayingStrengthAround(locus: Coordinate, state: PlayerVisibleState, distances: DistanceMap): Double = {
    val decayRate = 0.9
    def decayFunction(dist: Int) = Math.pow(decayRate, dist - 1.0)
    def strengthOfCell(coordinate: Coordinate): Double = state.board(coordinate) match {
      case OccupiedCellState(team, soldiers, _) =>
        (if (team == state.playingAsTeam) 1.0 else -1.0) * soldiers
      case _ => 0
    }

    distances.toList.collect {
      case ((c1, c2), dist) if c2 == locus => dist -> c1
    }.groupBy(_._1).map {
      case (None, _) => 0
      case (Some(dist), cellsAtDist) => decayFunction(dist) * cellsAtDist.map(c => strengthOfCell(c._2)).sum
    }.sum
  }

  private def scoreStrategy(strategy: ProposedStrategy, state: PlayerVisibleState, distances: DistanceMap): Double =
    strategy match {
      case AttackEnemyGeneral(p) => 1000.0 * decayingStrengthAround(p, state, distances)
      case TakeCityAt(p) => 10.0 * decayingStrengthAround(p, state, distances)
  }


  private def pickStrategies(state: PlayerVisibleState, distances: DistanceMap):
    List[(Double, ProposedStrategy)] =
      allPossibleStrategies(state).map(strategy => (scoreStrategy(strategy, state, distances), strategy))
        .sortBy(_._1)
        .take(maximumNotProtectiveConcurrentStrategies)

  private def neighbouringCells(coordinate: Coordinate): List[Coordinate] =
    List(
         Coordinate(coordinate.x - 1, coordinate.y), Coordinate(coordinate.x + 1, coordinate.y),
         Coordinate(coordinate.x, coordinate.y - 1), Coordinate(coordinate.x, coordinate.y + 1)
        )

  private def canOccupy(state: PlayerVisibleState, coordinate: Coordinate) =
    state.board.get(coordinate) match {
      case None => false
      case Some(MountainCell) => false
      case _ => true
    }

  private def allPossibleMoves(state: PlayerVisibleState, playingTeam: Team): List[ProposedAction] = {
    val sourceCells = state.board.toList.collect {
      case (coord, State.OccupiedCellState(team, soldiers, cellType)) if team == playingTeam && soldiers > 1 =>
        coord -> (soldiers > 2)
    }

    DoNothingAction :: sourceCells.flatMap { case (sourceCell, isHalfStrengthOption) =>
      neighbouringCells(sourceCell).filter(canOccupy(state, _)).flatMap { target =>
        ProposedAttackAction(sourceCell, target, halfStrength = false) ::
          (if (isHalfStrengthOption) List(ProposedAttackAction(sourceCell, target, halfStrength = true)) else List.empty)
      }
    }
  }

  /* TODO Need to work out whether this actually helps and either remove or re-enable.
  private def updateForLand(state: PlayerVisibleState): PlayerVisibleState = {
    val (generalLandBonus, newTurnsForBonus) =
      if (state.turnsUntilLandBonus == 0)
        (1, 25)
      else
        (0, state.turnsUntilLandBonus - 1)
      state.copy(turnsUntilLandBonus = newTurnsForBonus, board = state.board.map {
        case (coord, s@OccupiedCellState(_, soldiers, GeneralCell)) => coord -> s.copy(soldiers = soldiers + 1)
        case (coord, s@OccupiedCellState(_, soldiers, CityCell)) => coord -> s.copy(soldiers = soldiers + 1)
        case (coord, s@OccupiedCellState(_, soldiers, _)) => coord -> s.copy(soldiers = soldiers + generalLandBonus)
        case (coord, cellState) => coord -> cellState
      })
  }
  */

  private def predictImpactOf(action: ProposedAction, state: PlayerVisibleState, team: Team): PlayerVisibleState = {
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
                case (EmptyCell(cellType, _), true, _) => EmptyCell(cellType, -resultingStrength) -> None
                case (OccupiedCellState(destTeam, _, GeneralCell), false, true) => EmptyCell(NormalCell, 0) ->
                  Some(destTeam)
                case (OccupiedCellState(destTeam, _, cellType), false, true) => EmptyCell(cellType, 0) -> None
                case (OccupiedCellState(destTeam, _, cellType), false, false) =>
                  OccupiedCellState(destTeam, -resultingStrength, cellType) -> None
                case (OccupiedCellState(destTeam, _, GeneralCell), true, _) =>
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

  private def scoreStateForLand(state: PlayerVisibleState): Double = state.board.toList.map {
    case (_, OccupiedCellState(team, soldiers, CityCell)) if team == state.playingAsTeam => 250.0 + soldiers
    case (_, OccupiedCellState(team, soldiers, _)) if team == state.playingAsTeam => 10.0 + soldiers
  }.sum

  private def scoreStateForDefence(state: PlayerVisibleState, distances: DistanceMap): Double = {
    if (state.turn < 20)
      0 // Don't start thinking about defence until turn 20, as early attack is unlikely, and expansion is needed.
    else {
      val myGeneralPos = state.board.find {
        case (coord, OccupiedCellState(team, _, GeneralCell)) if team == state.playingAsTeam => true
        case _ => false
      }.get._1
      val generalProtection = state.board(myGeneralPos) match {
        case OccupiedCellState(_, strength, _) => strength
        case _ => 0
      }
      val totalTroops = state.board.toList.map {
        case (_, OccupiedCellState(team, strength, _)) if team == state.playingAsTeam => strength
        case _ => 0
      }.sum
      val protectionDeficit = 0.25 - (generalProtection + 0.0) / totalTroops

      val generalUnderoccupationPenalty = -Math.max(0.0, protectionDeficit) * 1E7

      val byDistanceFromGeneral : Map[Int, List[(Int, Coordinate)]] = distances.toList.collect {
        case ((from, to), Some(dist)) if from == myGeneralPos => dist -> to
      }.groupBy(_._1)

      case class ProtectionRing(distance: Int, myTotal: Int, enemyTotal: Int)

      val protectionPerimeter = 30
      val protectionRings = (1 to protectionPerimeter).toList.foldLeft(List(ProtectionRing(0, generalProtection, 0))) {
        case (Nil, _) => Nil
        case (l @ProtectionRing(_, myInnerRing, enemyInnerRing) :: _, atDistanceRing) =>
          val coords = byDistanceFromGeneral.getOrElse(atDistanceRing, List()).map(_._2)
          val myArmyInRing = coords.map(state.board.get).map {
            case Some(OccupiedCellState(team, soldiers, _)) if team == state.playingAsTeam => soldiers
            case _ => 0
          }.sum + myInnerRing
        val enemyInRing = coords.map(state.board.get).map {
          case Some(OccupiedCellState(team, soldiers, _)) if team != state.playingAsTeam => soldiers
          case _ => 0
        }.sum + enemyInnerRing
        ProtectionRing(atDistanceRing, myArmyInRing, enemyInRing) :: l
      }

      val safeRing : Int = protectionRings.filter(p => p.myTotal <= p.enemyTotal).map(p => p.distance).sorted
                                    .headOption.getOrElse(protectionPerimeter + 1)

      val protectionRingPenalty = protectionRings.map {
        case ProtectionRing(distance, myTotal, enemyTotal) =>
          val diff = myTotal - enemyTotal
          val underprotectionPenalty = if (diff <= 0) 1E5 else if (distance >= safeRing) 1E3 else 0
          diff * underprotectionPenalty / distance
      }.sum

      generalUnderoccupationPenalty + protectionRingPenalty
    }
  }

  private def scoreStateForStrategy(state: PlayerVisibleState, strategy: ProposedStrategy, distances: DistanceMap):
    Double = strategy match {
    case TakeCityAt(pos) => decayingStrengthAround(pos, state, distances)
    case AttackEnemyGeneral(pos) => decayingStrengthAround(pos, state, distances)
  }

  private def heuristicallyScoreState(state: PlayerVisibleState, strategies: List[(Double, ProposedStrategy)],
                                      distances: DistanceMap) =
    scoreStateForLand(state) + scoreStateForDefence(state, distances) + strategies.map {
      case (priority, strategy) => priority * scoreStateForStrategy(state, strategy, distances)
    }.sum

  private def findNextTeam(lastTeam: Team, state: PlayerVisibleState): Team = {
    val stillIn = state.scores.filter { case (_, score) => score.land > 0 }.toList.map(_._1)
    stillIn.find(t => t.teamId > lastTeam.teamId) match {
      case None => stillIn.head
      case Some(team) => team
    }
  }

  private def recursivelyScoreMinimax(state: PlayerVisibleState, lastTeam: Team, maximiseTeam: Team,
                                      strategies : List[(Double, ProposedStrategy)],
                                      distances: DistanceMap, depthLeft: Int): Double =
    if (state.scores(maximiseTeam).land == 0)
      -1E20 // Defeat
    else if (state.scores.count { case (_, score) => score.land > 0 } == 1)
      1E20 // Victory
    else if (depthLeft == 0)
      heuristicallyScoreState(state, strategies, distances)
    else {
      val nextTeam = findNextTeam(lastTeam, state)
      def evaluateMove(move: ProposedAction): Double =
        recursivelyScoreMinimax(state = predictImpactOf(move, state, nextTeam),
          lastTeam = nextTeam, maximiseTeam = maximiseTeam, strategies = strategies,
          distances = distances, depthLeft = depthLeft - 1)
      val moveScores = allPossibleMoves(state, nextTeam).map(evaluateMove)
      if (nextTeam == maximiseTeam)
        moveScores.max
      else
        moveScores.min
    }


  private def scoreAction(action: ProposedAction, state: PlayerVisibleState,
                          strategies: List[(Double, ProposedStrategy)],
                          distances: DistanceMap): Double = {
    val newState = predictImpactOf(action, state, state.playingAsTeam)
    recursivelyScoreMinimax(state = newState, lastTeam = newState.playingAsTeam, maximiseTeam = newState.playingAsTeam,
      distances = distances, strategies = strategies, depthLeft = 5)
  }

  def pickBestMove(state: PlayerVisibleState): ProposedAction = {
    val distances = FloydDistanceCalculator.distanceMapForPlayerState(state)
    val strategies = pickStrategies(state, distances)

    allPossibleMoves(state, state.playingAsTeam).sortBy(action =>
      scoreAction(action, state, strategies, distances)).head
  }
}
