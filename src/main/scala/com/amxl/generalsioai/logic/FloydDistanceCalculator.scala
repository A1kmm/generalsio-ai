package com.amxl.generalsioai.logic

import cats.Applicative
import cats.instances.all._
import com.amxl.generalsioai.models.State._

import scala.collection.immutable.Map
import scala.language.higherKinds

object FloydDistanceCalculator {
  type DistanceMap = Map[(Coordinate, Coordinate), Option[Int]]

  def distanceMapForPlayerState(state: PlayerVisibleState): DistanceMap = {
    val allCoords = (0 until state.size.x).flatMap(x => (0 until state.size.y).map(y => x -> y))
      .toList.map { case (x,y) => Coordinate(x, y) }
    val allPairs = allCoords.flatMap(x => allCoords.map(y => x -> y))

    val chooseMinOption : (Option[Int], Option[Int]) => Option[Int] = {
      case (None, None) => None
      case (Some(v), None) => Some(v)
      case (None, Some(v)) => Some(v)
      case (Some(vx), Some(vy)) => Some(Integer.min(vx, vy))
    }
    def isOffByOne(v1: Int, v2 : Int): Boolean = (v1 == v2 + 1) || (v1 == v2 - 1)
    val isNeighbouring : (Coordinate, Coordinate) => Boolean = {
      case (Coordinate(x1 : Int, y1 : Int), Coordinate(x2 : Int, y2 : Int)) =>
        (x1 == x2 && isOffByOne(y1, y2)) || (y1 == y2 && isOffByOne(x1, x2))
    }
    def entryCost(c: Coordinate): Option[Int] =
      state.board(c) match {
        case UnknownCell => Some(1)
        case OccupiedCellState(_, _, _) => Some(1)
        case EmptyCell(_, _) => Some(1)
        case MountainCell => None
      }

    def lift2[A[_] : Applicative, B, C, D](f: (B, C) => D): (A[B], A[C]) => A[D] =
      Function.uncurried(Applicative[A].lift(f.curried).andThen(Applicative[A].ap))
    val addOption: (Option[Int], Option[Int]) => Option[Int] = lift2[Option, Int, Int, Int](_+_)

    def go(map0: DistanceMap, kNode: Coordinate): DistanceMap =
      allPairs.map { case p@(c1, c2) =>
          p -> chooseMinOption(map0(p), addOption(map0(c1 -> kNode), map0(kNode -> c2)))
      }.toMap

    val initialMap : DistanceMap = allPairs.map {
      case p@(c1, c2) if c1 == c2 => p -> Some(0)
      case p@(c1, c2) if isNeighbouring(c1, c2) => p -> entryCost(c2)
      case p => p -> None
    }.toMap

    allCoords.foldLeft(initialMap)(go)
  }
}
