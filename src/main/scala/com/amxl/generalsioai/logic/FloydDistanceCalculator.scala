package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.State._

import spire.syntax.cfor._

object FloydDistanceCalculator {
  type DistanceMap = Coordinate => Coordinate => Option[Int]

  def distanceMapForPlayerState(state: PlayerVisibleState): DistanceMap = {
    val coordinateCount = state.size.y * state.size.x

    def idxToCoord(idx: Int): Coordinate = Coordinate(x = idx % state.size.x, y = idx / state.size.x)
    def coordToIdx(coord: Coordinate): Int = coord.y * state.size.x + coord.x

    def plusWithNegAsNone(a: Int, b: Int) = if (a < 0 || b < 0) -1 else a + b

    // Begin impure code (no effects escape)
    val floydTemporaryArray: Array[Int] = Array.fill(coordinateCount * coordinateCount)(-1)
    cfor(0)(_ < coordinateCount, _ + 1) { i =>
      floydTemporaryArray(i * coordinateCount + i) = 0
    }
    cfor(0)(_ < coordinateCount, _ + 1) { i =>
      val coord = idxToCoord(i)
      state.board(coord) match {
        case MountainCell =>
        case _ =>
          Seq(coord.copy(x = coord.x - 1), coord.copy(x = coord.x + 1), coord.copy(y = coord.y - 1),
              coord.copy(y = coord.y + 1)).filter(j => j.x >= 0 && j.x < state.size.x && j.y >= 0 &&
                                                  j.y < state.size.y).
            foreach((j: Coordinate) =>
              state.board(j) match {
                case MountainCell =>
                case _ => floydTemporaryArray(coordToIdx(j) * coordinateCount + i) = 1
              }
            )
      }
    }
    cfor(0)(_ < coordinateCount, _ + 1) { k =>
      cfor(0)(_ < coordinateCount, _ + 1) { i =>
        cfor(0)(_ < coordinateCount, _ + 1) { j =>
          val indirectWeight =
            plusWithNegAsNone(floydTemporaryArray(i * coordinateCount + k),
              floydTemporaryArray(k * coordinateCount + j))
          val offset = i * coordinateCount + j
          if (indirectWeight != -1 &&
            (floydTemporaryArray(offset) == -1 || floydTemporaryArray(offset) > indirectWeight))
            floydTemporaryArray(offset) = indirectWeight
        }
      }
    }
    // End impure code

    val floydArray = floydTemporaryArray.map { x =>
      if (x < 0)
        None
      else
        Some(x)
    }

    (c1) => (c2) => floydArray(coordToIdx(c1) * coordinateCount + coordToIdx(c2))
  }
}
