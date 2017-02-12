package com.amxl.generalsioai.logic

import java.util
import java.util.{Random => jRandom}

import com.amxl.generalsioai.models.{DoNothingAction, ProposedAction, ProposedAttackAction}
import com.amxl.generalsioai.models.State._
import org.uncommons.watchmaker.framework._
import MoveFinder._
import ActionImpactPredictor._
import org.uncommons.maths.random.Probability
import org.uncommons.watchmaker.framework.factories.AbstractCandidateFactory
import org.uncommons.watchmaker.framework.operators.{Replacement, SplitEvolution}
import org.uncommons.watchmaker.framework.selection.TournamentSelection
import org.uncommons.watchmaker.framework.termination.{ElapsedTime, Stagnation}

import scala.collection.convert.{AsJavaConverters, AsScalaConverters}
import scala.util.Random

object EvolutionaryMoveRanker extends AsJavaConverters with AsScalaConverters {
  private case class StrategicMemory(lastIndividuals : Iterable[Individual])
  private val futureConsiderationWindow = 30

  private case class Individual(actions: Seq[ProposedAction])
  private sealed trait CompassPoint
  private case object North extends CompassPoint
  private case object East extends CompassPoint
  private case object South extends CompassPoint
  private case object West extends CompassPoint

  private def directionOfAttack(action: ProposedAttackAction): CompassPoint = {
    () match {
      case _ if action.destination.y < action.source.y => North
      case _ if action.destination.x < action.source.x => East
      case _ if action.destination.y > action.source.y => South
      case _ => West
    }
  }

  private def l1NormDistance(point1: Coordinate, point2: Coordinate): Int =
    Math.abs(point1.y - point2.y) + Math.abs(point1.x - point2.x)

  private def canonicaliseIndividual(in: Individual, state: PlayerVisibleState): Individual = {
    case class CanonicaliseState(curState: PlayerVisibleState, canonicalActions: List[ProposedAction])

    val res = in.actions.foldLeft(CanonicaliseState(curState = state, canonicalActions = List.empty)) {
      case (CanonicaliseState(curState, canonicalActions), originalAction) =>
        val canonicalAction = originalAction match {
          case DoNothingAction => DoNothingAction
          case attackAction: ProposedAttackAction =>
            val (origOk, halfOk) = state.board(attackAction.source) match {
              case OccupiedCellState(team, soldiers, _) if team == state.playingAsTeam && soldiers > 1 =>
                (canOccupy(state, attackAction.destination), soldiers > 2)
              case _ => (false, false)
            }

            if (origOk)
              attackAction.copy(halfStrength = attackAction.halfStrength && halfOk)
            else {
              val origDirection = directionOfAttack(attackAction)
              val allMoves = allPossibleMoves(curState, curState.playingAsTeam)
              allMoves.map {
                case DoNothingAction => DoNothingAction -> 1000000000
                case act@ProposedAttackAction(from, _, halfStrength) => act ->
                  (l1NormDistance(from, attackAction.source) +
                    (if (directionOfAttack(act) == origDirection) 0 else 1000) +
                    (if (halfStrength == attackAction.halfStrength) 0 else 1))
              }.minBy(_._2)._1
            }
        }
        CanonicaliseState(updateForLand(predictImpactOf(canonicalAction, curState, curState.playingAsTeam)),
          canonicalAction :: canonicalActions)
    }

    Individual(actions = res.canonicalActions.reverse)
  }

  private def haveWeLost(state: PlayerVisibleState): Boolean =
    state.scores.exists {
      case (team, score) => team == state.playingAsTeam && score.land == 0
    }

  private def haveWeWon(state: PlayerVisibleState): Boolean =
    !state.scores.exists {
      case (team, score) => team != state.playingAsTeam && score.land > 0
    }

  private def scoreMyLandAndArmy(state: PlayerVisibleState): Double =
    state.board.toSeq.collect {
      case (_, OccupiedCellState(team, soldiers, cellType)) if team == state.playingAsTeam =>
        soldiers + 1 + (if (cellType == CityCell) 100.0 else 0.0)
    }.sum + 0.0

  private def penaltyForGeneralDanger(generalPos: Coordinate, state: PlayerVisibleState): Double =
    1000.0 * state.board.toSeq.collect {
      case (coord, OccupiedCellState(team, soldiers, _))
        if team != state.playingAsTeam && l1NormDistance(coord, generalPos) < 6.0 =>
          soldiers / l1NormDistance(coord, generalPos)
    }.sum

  private def fitnessEvaluator(state: PlayerVisibleState) = new FitnessEvaluator[Individual] {
    private val generalPos = state.board.toSeq.find {
      case (_, OccupiedCellState(team, _, GeneralCell)) if team == state.playingAsTeam => true
      case _ => false
    }.map(_._1).getOrElse(Coordinate(0, 0))

    override def isNatural: Boolean = true
    override def getFitness(in: Individual, population: util.List[_ <: Individual]): Double = {
      case class ScoreState(curState: PlayerVisibleState, fitness: Double, gameOver: Boolean)

      val res = in.actions.zipWithIndex.foldLeft(ScoreState(curState = state, fitness = 0.0, gameOver = false)) {
        case (st@ScoreState(curState, prevFitness, wasGameOver), (action, idx)) =>
          if (wasGameOver)
            st
          else {
            val (fitnessComponent, gameOver) =
              if (haveWeLost(curState))
                (-1E20, true)
              else if (haveWeWon(curState))
                (1E20, true)
              else
                (scoreMyLandAndArmy(curState) - penaltyForGeneralDanger(generalPos, curState), false)

            ScoreState(updateForLand(predictImpactOf(action, curState, curState.playingAsTeam)),
              prevFitness + fitnessComponent / (idx + 1), gameOver)
          }
      }

      res.fitness + 1E9
    }
  }

  private def sample1[A](l: Seq[A]): A = l(Random.nextInt(l.size))

  private def candidateFactory(state: PlayerVisibleState) : CandidateFactory[Individual] =
    new AbstractCandidateFactory[Individual] {
      override def generateRandomCandidate(rng: jRandom): Individual = {
        Individual((1 to futureConsiderationWindow).foldLeft((state, List.empty[ProposedAction])) {
          case ((curState, curList), _) =>
            val action = sample1(allPossibleMoves(curState, state.playingAsTeam))
            (updateForLand(predictImpactOf(action, curState, state.playingAsTeam)), action :: curList)
        }._2.reverse)
      }
    }

  private def mutateOne(f: (PlayerVisibleState, Individual) => Individual)(state: PlayerVisibleState):
    EvolutionaryOperator[Individual] =
      (selectedCandidates: util.List[Individual], _: jRandom) =>
        seqAsJavaList(collectionAsScalaIterable(selectedCandidates).map {
          individual => canonicaliseIndividual(f(state, individual), state)
        }.toSeq)

  private val swapAction : PlayerVisibleState => EvolutionaryOperator[Individual] = mutateOne {
    (_, individual) => {
      val position = sample1(0 until (futureConsiderationWindow - 1))
      val (head, a1::a2::tail) = individual.actions.toList.splitAt(position)
      Individual(head ++ (a2::a1::tail))
    }
  }

  private val replaceAction : PlayerVisibleState => EvolutionaryOperator[Individual] = mutateOne {
    (state, individual) =>
      val position = sample1(0 until futureConsiderationWindow)
      val (head, _::tail) = individual.actions.toList.splitAt(position)
      val stateAtReplace = head.foldLeft(state) { case (curState, action) =>
        updateForLand(predictImpactOf(action, curState, state.playingAsTeam))
      }
      val newMove = sample1(allPossibleMoves(stateAtReplace, state.playingAsTeam))
      Individual(head ++ (newMove::tail))
  }

  private def replaceIndividual(state: PlayerVisibleState) =
    new Replacement[Individual](candidateFactory(state), Probability.ONE)

  private def overallEvolutionaryStrategy(state: PlayerVisibleState) = new SplitEvolution[Individual](
    swapAction(state),
    new SplitEvolution[Individual](
      replaceAction(state),
      replaceIndividual(state),
      0.8
    ),
    0.5
  )
  private val terminateStagnant = new Stagnation(10, true)
  private val terminateTime = new ElapsedTime(400)

  private def lastIndividualToThisTurn(state: PlayerVisibleState, individual: Individual): Individual = {
    val finalState = individual.actions.foldLeft(state) { case (curState, action) =>
        updateForLand(predictImpactOf(action, curState, state.playingAsTeam))
    }
    Individual(individual.actions.tail :+ sample1(allPossibleMoves(finalState, state.playingAsTeam)))
  }

  private def pickBestMove(memory: StrategicMemory, state: PlayerVisibleState): (StrategicMemory, ProposedAction) = {
    val engine = new GenerationalEvolutionEngine[Individual](
      candidateFactory(state),
      overallEvolutionaryStrategy(state),
      fitnessEvaluator(state),
      new TournamentSelection(new Probability(0.9)),
      new jRandom()
    )
    engine.addEvolutionObserver(new EvolutionObserver[Individual] {
      override def populationUpdate(data: PopulationData[_ <: Individual]): Unit =
        println("Generation: " + data.getGenerationNumber + " best fitness: " + data.getBestCandidateFitness)
    })
    val population = collectionAsScalaIterable(engine.evolvePopulation(10, 2,
      asJavaCollection(memory.lastIndividuals.map(
        i => lastIndividualToThisTurn(state: PlayerVisibleState, i))),
      terminateStagnant, terminateTime))
    val bestIndividual = population.maxBy(i => i.getFitness).getCandidate
    (memory.copy(lastIndividuals = population.map(_.getCandidate)), bestIndividual.actions.head)
  }

  private def goAi(memory: StrategicMemory): GameBot.Ai = GameBot.Ai(state => {
    val (newMemory, action) = pickBestMove(memory, state)
    (action, goAi(newMemory))
  })

  val ai : GameBot.Ai = goAi(StrategicMemory(lastIndividuals = Seq()))
}