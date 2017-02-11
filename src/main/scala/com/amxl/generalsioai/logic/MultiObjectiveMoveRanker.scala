package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.{ProposedStrategy, _}
import com.amxl.generalsioai.models.State._
import FloydDistanceCalculator.DistanceMap
import MoveFinder._
import ActionImpactPredictor._

import scala.util.Random

object MultiObjectiveMoveRanker {
  private val maximumNotProtectiveConcurrentStrategies = 1
  private case class StrategicMemory(randomObsession: Option[Coordinate] = None,
                                     lastStrategies: Seq[ProposedStrategy] = Seq())

  private def allPossibleStrategies(state: PlayerVisibleState, strategy: StrategicMemory): Seq[ProposedStrategy] = {
    strategy.randomObsession.map(c => Seq(TakeCellAt(c))).getOrElse(Seq.empty) ++
    state.board.toSeq.collect {
      case (p, OccupiedCellState(team, _, GeneralCell)) if team != state.playingAsTeam =>
        TakeCellAt(p)
      case (p, OccupiedCellState(team, _, CityCell)) if team != state.playingAsTeam =>
        TakeCellAt(p)
      case (p, EmptyCell(CityCell, _)) => TakeCellAt(p)
    }
  }

  private def updateObsession(state: PlayerVisibleState, memory: StrategicMemory): StrategicMemory = {
    val coordIfUsable : Option[Coordinate] = memory.randomObsession.flatMap { coord =>
      state.board(coord) match {
        case OccupiedCellState(team, _, _) if team == state.playingAsTeam => None
        case MountainCell => None
        case _ => Some(coord)
      }
    }
    val newObsession : Option[Coordinate] = coordIfUsable match {
      case None =>
        Random.shuffle(state.board.toSeq.filter { case (coord, cellState) =>
          cellState match {
            case UnknownCell => true
            case OccupiedCellState(team, _, _) if team != state.playingAsTeam => true
            case _ => false
          }
        }).headOption.map(_._1)
      case x => x
    }
    memory.copy(randomObsession = newObsession)
  }

  private def decayingStrengthAround(locus: Coordinate, state: PlayerVisibleState, distances: DistanceMap): Double = {
    val decayRate = 0.6
    def decayFunction(dist: Int) = Math.pow(decayRate, dist - 1.0)
    def strengthOfCell(coordinate: Coordinate): Double = state.board(coordinate) match {
      case OccupiedCellState(team, soldiers, _) =>
        (if (team == state.playingAsTeam) 1.0 else -1.0) * soldiers
      case _ => 0
    }

    val distanceFromLocus = distances(locus)
    allCoords(state).map(v => distanceFromLocus(v) -> v).groupBy(_._1).map {
      case (None, _) => 0
      case (Some(dist), cellsAtDist) => decayFunction(dist) * cellsAtDist.map(c => strengthOfCell(c._2)).sum
    }.sum
  }

  private def scoreStrategy(strategy: ProposedStrategy, state: PlayerVisibleState, distances: DistanceMap,
                            lastStrategies: Seq[ProposedStrategy]): Double =
    (if (lastStrategies.contains(strategy)) 1.2 else 1.0) *
      Math.max(1, strategy match {
          case TakeCellAt(p) =>
            val strength = decayingStrengthAround(p, state, distances)
            state.board(p) match {
              case OccupiedCellState(_, _, GeneralCell) => 1000000.0 * strength
              case OccupiedCellState(_, _, CityCell) => 100.0 * strength
              case EmptyCell(CityCell, _) => 100.0 * strength
              case _ => strength
            }
        })


  private def pickStrategies(state: PlayerVisibleState, distances: DistanceMap, memory: StrategicMemory):
  (StrategicMemory, Seq[(Double, ProposedStrategy)]) = {
    val newMemory = updateObsession(state, memory)
    val strategies = allPossibleStrategies(state, memory)
         .map(strategy => (scoreStrategy(strategy, state, distances, memory.lastStrategies), strategy))
         .sortBy(_._1)
         .take(maximumNotProtectiveConcurrentStrategies)
    (newMemory.copy(lastStrategies = strategies.map(_._2)), strategies)
  }

  private def scoreStateForLand(state: PlayerVisibleState): Double = state.board.toSeq.map {
    case (_, OccupiedCellState(team, soldiers, CityCell)) if team == state.playingAsTeam => 250.0 + soldiers
    case (_, OccupiedCellState(team, soldiers, _)) if team == state.playingAsTeam => 10.0 + soldiers
    case _ => 0
  }.sum

  private def scoreStateForDefence(state: PlayerVisibleState, distances: DistanceMap): Double = {
    if (state.turn < 100)
      0 // Don't start thinking about defence until turn 100, as early attack is unlikely, and expansion is needed.
    else {
      val myGeneralPos = state.board.find {
        case (coord, OccupiedCellState(team, _, GeneralCell)) if team == state.playingAsTeam => true
        case _ => false
      }.get._1
      val generalProtection = state.board(myGeneralPos) match {
        case OccupiedCellState(_, strength, _) => strength
        case _ => 0
      }
      val totalTroops = state.board.toSeq.map {
        case (_, OccupiedCellState(team, strength, _)) if team == state.playingAsTeam => strength
        case _ => 0
      }.sum
      val protectionDeficit = 0.1 - (generalProtection + 0.0) / totalTroops

      val generalUnderoccupationPenalty = -Math.max(0.0, protectionDeficit) * 1E7

      val distanceFromGeneral = distances(myGeneralPos)
      val byDistanceFromGeneral : Map[Option[Int], Seq[(Coordinate, Option[Int])]] =
        allCoords(state).map(v => v -> distanceFromGeneral(v)).groupBy(_._2)

      case class ProtectionRing(distance: Int, myTotal: Int, enemyTotal: Int)

      val protectionPerimeter = 5
      val protectionRings = (1 to protectionPerimeter).foldLeft(List(ProtectionRing(0, generalProtection, 0))) {
        case (Nil, _) => Nil
        case (l @ProtectionRing(_, myInnerRing, enemyInnerRing) :: _, atDistanceRing) =>
          val coords = byDistanceFromGeneral.getOrElse(Some(atDistanceRing), List()).map(_._1)
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
          if (distance == 0)
            0
          else
            diff * underprotectionPenalty / distance
      }.sum

      generalUnderoccupationPenalty + protectionRingPenalty
    }
  }

  private def findCostForWeight(targetWeight: Double, costsAndWeights: Seq[(Double, Double)]): Double = {
    val result = costsAndWeights.foldLeft((targetWeight, 0.0, 0)) { (state, costWeight) =>
      (state, costWeight) match {
        case ((weightLeft, costSoFar, countSoFar), (nextCost, nextWeight)) =>
          if (weightLeft <= 0)
            state
          else
            (weightLeft - Math.max(1.0, nextWeight), costSoFar + nextCost, countSoFar + 1)
      }
    }
    result._2 / result._3
  }

  private def scoreStateForStrategy(state: PlayerVisibleState, strategy: ProposedStrategy, distances: DistanceMap,
                                    attackCostDistances: DistanceMap):
    Double = strategy match {
      case TakeCellAt(pos) =>
        val costAndWeight = state.board.toSeq.collect {
          case (coord, OccupiedCellState(team, soldiers, _)) if team == state.playingAsTeam =>
            distances(coord)(pos).map(_ + 0.0).getOrElse(1E6) ->
              (soldiers - attackCostDistances(coord)(pos).map(_ + 0.0).getOrElse(1E6))
        }.sortBy(-_._2)
        state.board(pos) match {
          case OccupiedCellState(team, _, _) if team == state.playingAsTeam => 1E7
          case OccupiedCellState(_, soldiers, _) => 1E3-findCostForWeight(1.5 * soldiers + 1.0, costAndWeight)
          case EmptyCell(_, soldiers) => 1E3-findCostForWeight(1.5 * soldiers + 1.0, costAndWeight)
          case _ => 1E3-findCostForWeight(10.0, costAndWeight)
        }
    }

  private def heuristicallyScoreState(state: PlayerVisibleState, strategies: Seq[(Double, ProposedStrategy)],
                                      distances: DistanceMap, attackCostDistances: DistanceMap) =
    scoreStateForLand(state) + scoreStateForDefence(state, distances) + strategies.map {
      case (priority, strategy) => priority * scoreStateForStrategy(state, strategy, distances, attackCostDistances)
    }.sum

  private def findNextTeam(lastTeam: Team, state: PlayerVisibleState): Team = {
    val stillIn = state.scores.filter { case (_, score) => score.land > 0 }.toSeq.map(_._1)
    stillIn.find(t => t.teamId > lastTeam.teamId) match {
      case None => stillIn.head
      case Some(team) => team
    }
  }

  private def recursivelyScoreMinimax(state: PlayerVisibleState, lastTeam: Team, maximiseTeam: Team,
                                      strategies : Seq[(Double, ProposedStrategy)],
                                      distances: DistanceMap, attackCostDistances: DistanceMap,
                                      depthLeft: Int): Double =
    if (state.scores(maximiseTeam).land == 0)
      -1E20 // Defeat
    else if (state.scores.count { case (_, score) => score.land > 0 } == 1)
      1E20 // Victory
    else if (depthLeft == 0)
      heuristicallyScoreState(state, strategies, distances, attackCostDistances)
    else {
      val nextTeam = findNextTeam(lastTeam, state)
      def evaluateMove(move: ProposedAction): Double =
        recursivelyScoreMinimax(state = predictImpactOf(move, state, nextTeam),
          lastTeam = nextTeam, maximiseTeam = maximiseTeam, strategies = strategies,
          distances = distances, attackCostDistances = attackCostDistances, depthLeft = depthLeft - 1)
      val moveScores = allPossibleMoves(state, nextTeam).map(evaluateMove)
      if (nextTeam == maximiseTeam)
        moveScores.max
      else
        moveScores.min
    }


  private def scoreAction(action: ProposedAction, state: PlayerVisibleState,
                          strategies: Seq[(Double, ProposedStrategy)],
                          distances: DistanceMap, attackCostDistances: DistanceMap): Double = {
    val newState = predictImpactOf(action, state, state.playingAsTeam)
    val inactionPenalty = if (action == DoNothingAction) -1 else 0
    recursivelyScoreMinimax(state = newState, lastTeam = newState.playingAsTeam, maximiseTeam = newState.playingAsTeam,
      distances = distances, attackCostDistances = attackCostDistances, strategies = strategies,
      depthLeft = 0) + inactionPenalty
  }

  private def pickBestMove(memory: StrategicMemory, state: PlayerVisibleState): (StrategicMemory, ProposedAction) = {
    val distances = FloydDistanceCalculator.distanceMapForPlayerState(state)
    val attackCostDistances = FloydDistanceCalculator.attackCostDistanceMapForPlayerState(state)
    val (newMemory, strategies) = pickStrategies(state, distances, memory)
    println("Strategies: " + strategies)

    val scoredMoves = allPossibleMoves(state, state.playingAsTeam).par.map(action =>
      action -> scoreAction(action, state, strategies, distances, attackCostDistances)
    )

    (newMemory, scoredMoves.seq.sortBy(-_._2).head._1)
  }

  private def goAi(memory: StrategicMemory): GameBot.Ai = GameBot.Ai(state => {
    val (newMemory, action) = pickBestMove(memory, state)
    (action, goAi(newMemory))
  })

  val ai : GameBot.Ai = goAi(StrategicMemory())
}
