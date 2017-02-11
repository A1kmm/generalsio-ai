package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction}
import com.amxl.generalsioai.models.State._
import MoveFinder._

object PhaseBasedMoveRanker {
  private sealed trait GamePhase
  private case object GrowArmy extends GamePhase
  private case object FindEnemy extends GamePhase
  private case object KillEnemy extends GamePhase
  private case object KillGeneral extends GamePhase

  private case class StrategicMemory(strategicPhase: GamePhase = GrowArmy)

  private def pickPhase(memory: StrategicMemory, state: PlayerVisibleState): GamePhase = {
    val myArmy = state.scores.toSeq.collect {
      case (team, score) if team == state.playingAsTeam => score.army
    }.sum
    val theirArmy = state.scores.toSeq.collect {
      case (team, score) if team != state.playingAsTeam => score.army
    }.sum

    val killOk = ((0.0 + myArmy - 20.0) / theirArmy) >= (if (memory.strategicPhase == GrowArmy) 1.2 else 1.0)

    if (killOk) {
      val knowGeneral = state.board.exists {
        case (_, OccupiedCellState(team, _, GeneralCell)) if team != state.playingAsTeam => true
        case _ => false
      }
      val knowEnemy = state.board.exists {
        case (_, OccupiedCellState(team, _, _)) if team != state.playingAsTeam => true
        case _ => false
      }


      if (knowGeneral)
        KillGeneral
      else if (knowEnemy)
        KillEnemy
      else
        FindEnemy
    } else
      GrowArmy
  }

  private def takeUntilSum[A](target: Int, s: Seq[(Int, A)]): Option[Seq[(Int, A)]] = {
    val result = s.foldLeft(target -> List.empty[(Int, A)]) {
      case (x@(curTarget, _), _) if curTarget <= 0 => x
      case (x@(curTarget, curList), item@(v, _)) =>
        (curTarget - v, item :: curList)
    }
    if (result._1 > 0)
      None
    else
      Some(result._2.reverse)
  }

  private def optimalRouteMove(source: Coordinate, dest: Coordinate, dists: FloydDistanceCalculator.DistanceMap):
    ProposedAction = {
    val nextHop = neighbouringCells(source).flatMap { neighbour =>
        dists(source)(neighbour).map(dist => neighbour -> dist)
    }.minBy(_._2)._1
    ProposedAttackAction(source, nextHop, halfStrength = false)
  }

  private def pickBestMoveWithWeight(extraBonus: Coordinate => Double)(state: PlayerVisibleState): ProposedAction = {
    val attackMap = FloydDistanceCalculator.attackCostDistanceMapForPlayerState(state)

    val origins = state.board.toSeq.collect {
      case (coord, OccupiedCellState(team, soldiers, _)) if team == state.playingAsTeam =>
        coord -> soldiers
    }
    val originsMap = Map(origins :_*)

    def isOccupiedAdjacent(pos: Coordinate): Boolean =
      neighbouringCells(pos).exists((neighbourPos) => originsMap.contains(neighbourPos))

    val targetOptions = state.board.toSeq.collect {
      case (coord, OccupiedCellState(team, soldiers, _)) if team != state.playingAsTeam => coord
      case (coord, EmptyCell(_, _)) => coord
    }
    val targets = targetOptions.filter(isOccupiedAdjacent)

    val dist = targets.flatMap(targetCoord => {
      val cellTypeBonus : Double = (state.board(targetCoord) match {
        case OccupiedCellState(team, _, _) if team == state.playingAsTeam => 1.0
        case OccupiedCellState(_, _, GeneralCell) => 1000.0
        case OccupiedCellState(_, _, CityCell) => 25.0
        case EmptyCell(CityCell, _) => 25.0
        case _ => 1
      }) * extraBonus(targetCoord)

      val targetCost = state.board(targetCoord) match {
        case EmptyCell(_, soldiers) => soldiers + 1
        case OccupiedCellState(team, soldiers, _) if team != state.playingAsTeam => soldiers + 1
        case _ => 1
      }

      val optionOriginsToInvolve = takeUntilSum(targetCost, origins.map {
        case (originCoord, soldiers) => soldiers -> (attackMap(originCoord)(targetCoord).get -> originCoord)
      }.sortBy {
        case (_, (attackDist, _)) => attackDist
      })
      optionOriginsToInvolve.map { originsToInvolve : Seq[(Int, (Int, Coordinate))] =>
          val originMaxCost = originsToInvolve.map {
            case (_, (cost, _)) => cost
          }.max
          val (_, (_, firstMove)) = originsToInvolve.last
          (originMaxCost / cellTypeBonus) -> (firstMove -> targetCoord)
      }
    })

    if (dist.isEmpty)
      DoNothingAction
    else {
      val bestPath = dist.maxBy(_._1)._2
      println("Best path " + bestPath)
      optimalRouteMove(bestPath._1, bestPath._2, attackMap)
    }
  }

  private val pickBestGrowMove : PlayerVisibleState => ProposedAction = pickBestMoveWithWeight((_) => 1)
  private def pickBestFindMove(state: PlayerVisibleState): ProposedAction = {
    val myGeneral = state.board.find {
      case (coord, OccupiedCellState(team, _, GeneralCell)) if team == state.playingAsTeam => true
      case _ => false
    }.map(_._1).getOrElse(Coordinate(0, 0))

    val dists = FloydDistanceCalculator.distanceMapForPlayerState(state)

    pickBestMoveWithWeight(coord => dists(myGeneral)(coord).getOrElse(0) + 0.0)(state)
  }

  private def pickBestKillEnemyMove(state: PlayerVisibleState): ProposedAction = {
    val attackMap = FloydDistanceCalculator.attackCostDistanceMapForPlayerState(state)

    def computeAttackBenefit(c: OccupiedCellState): Double = {
      c match {
        case OccupiedCellState(_, soldiers, GeneralCell) => 1E9
        case OccupiedCellState(_, soldiers, CityCell) => soldiers + 25.0
        case OccupiedCellState(_, soldiers, _) => soldiers + 1.0
      }
    }

    val enemyCellAttackBenefit : Seq[(Coordinate, (Int, Double))] = state.board.toSeq.collect {
      case (coord, c@OccupiedCellState(team, soldiers, _)) if team != state.playingAsTeam =>
        coord -> (soldiers -> computeAttackBenefit(c))
    }

    val origins = state.board.toSeq.collect {
      case (coord, OccupiedCellState(team, soldiers, _)) if soldiers > 1 && team == state.playingAsTeam =>
        coord -> soldiers
    }
    val originsMap = Map(origins :_*)

    def isOccupiedAdjacent(pos: Coordinate): Boolean =
      neighbouringCells(pos).exists(neighbourPos => originsMap.contains(neighbourPos))

    val enemyCellCostBenefitRatio : Seq[((Coordinate, Coordinate), Double)] = enemyCellAttackBenefit.flatMap {
      case (targetCoord, (targetCost, benefit)) =>
        val optionOriginsToInvolve = takeUntilSum(targetCost, origins.map {
          case (originCoord, soldiers) => soldiers -> (attackMap(originCoord)(targetCoord).get -> originCoord)
        }.sortBy {
          case (_, (attackDist, _)) => attackDist
        })
        optionOriginsToInvolve map (originsToInvolve => {
          val originMaxCost = originsToInvolve.map {
            case (_, (cost, _)) => cost
          }.max
          val (_, (_, firstMove)) = originsToInvolve.last
          (firstMove -> targetCoord) -> (originMaxCost / benefit)
        })
    }
    if (enemyCellCostBenefitRatio.isEmpty)
      DoNothingAction
    else {
      val bestPath: (Coordinate, Coordinate) = enemyCellCostBenefitRatio.minBy(_._2)._1
      optimalRouteMove(bestPath._1, bestPath._2, attackMap)
    }
  }

  private def pickBestKillGeneralMove(state: PlayerVisibleState): ProposedAction = {
    val attackMap = FloydDistanceCalculator.attackCostDistanceMapForPlayerState(state)

    val (theirGeneral, theirGeneralStrength) = state.board.toSeq.collect {
      case (coord, OccupiedCellState(team, soldiers, GeneralCell)) if team != state.playingAsTeam => coord -> soldiers
    }.head

    val origins = state.board.toSeq.collect {
      case (coord, OccupiedCellState(team, soldiers, _)) if soldiers > 1 && team == state.playingAsTeam =>
        coord -> soldiers
    }
    val originsMap = Map(origins :_*)

    def isOccupiedAdjacent(pos: Coordinate): Boolean =
      neighbouringCells(pos).exists(neighbourPos => originsMap.contains(neighbourPos))

    val optionOriginsToInvolve = takeUntilSum(theirGeneralStrength, origins.map {
      case (originCoord, soldiers) => soldiers -> (attackMap(originCoord)(theirGeneral).get -> originCoord)
    }.sortBy {
      case (_, (attackDist, _)) => attackDist
    })

    optionOriginsToInvolve match {
      case None => pickBestKillEnemyMove(state) // Should be impossible anyway if we select this phase...
      case Some(originsToInvolve) =>
        val originMaxCost = originsToInvolve.map {
          case (_, (cost, _)) => cost
        }.max
        val (_, (_, firstMove)) = originsToInvolve.last
        optimalRouteMove(firstMove, theirGeneral, attackMap)
    }
  }

  private def pickBestMove(memory: StrategicMemory, state: PlayerVisibleState): (StrategicMemory, ProposedAction) = {
    val phase = pickPhase(memory, state)
    val move = phase match {
      case GrowArmy => pickBestGrowMove(state)
      case FindEnemy => pickBestFindMove(state)
      case KillEnemy => pickBestKillEnemyMove(state)
      case KillGeneral => pickBestKillGeneralMove(state)
    }
    (memory.copy(strategicPhase = phase), move)
  }

  private def goAi(memory: StrategicMemory): GameBot.Ai = GameBot.Ai(state => {
    val (newMemory, action) = pickBestMove(memory, state)
    (action, goAi(newMemory))
  })

  val ai : GameBot.Ai = goAi(StrategicMemory())
}
