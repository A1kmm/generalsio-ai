package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.State._

import scala.collection.immutable.Map

object FloydDistanceCalculator {
  type DistanceMap = Map[Coordinate, Map[Coordinate, Option[Int]]]

  def distanceMapForPlayerState(state: PlayerVisibleState): DistanceMap = {
    val coordinateCount = state.size.y * state.size.x

    def idxToCoord(idx: Int): Coordinate = Coordinate(x = idx % state.size.x, y = idx / state.size.x)

    def plusWithNegAsNone(a: Int, b: Int) = if (a < 0 || b < 0) -1 else a + b

    // Begin impure code (no effects escape)
    val floydTemporaryArray: Array[Int] = Array.fill(coordinateCount * coordinateCount)(-1)
    (0 until coordinateCount).foreach((i: Int) =>
      floydTemporaryArray(i * coordinateCount + i) = 0
    )
    (0 until coordinateCount).foreach((i: Int) =>
      state.board(idxToCoord(i)) match {
        case MountainCell =>
        case _ =>
          Seq(i - 1, i + 1, i - state.size.x, i + state.size.x).filter(j => j >= 0 && j < coordinateCount).
            foreach((j: Int) => floydTemporaryArray(j * coordinateCount + i) = 1)
      }
    )
    (0 until coordinateCount).foreach((k: Int) =>
      (0 until coordinateCount).foreach((i: Int) =>
        (0 until coordinateCount).foreach((j: Int) => {
          val indirectWeight =
            plusWithNegAsNone(floydTemporaryArray(i * coordinateCount + k),
              floydTemporaryArray(k * coordinateCount + j))
          val offset = i * coordinateCount + j
          if (indirectWeight != -1 &&
              (floydTemporaryArray(offset) == -1 || floydTemporaryArray(offset) > indirectWeight))
            floydTemporaryArray(offset) = indirectWeight
        })
      )
    )
    // End impure code

    (0 until coordinateCount).map((i: Int) =>
      idxToCoord(i) -> (0 until coordinateCount).map((j: Int) =>
        idxToCoord(j) -> {
          val offset = i * coordinateCount + j
          if (floydTemporaryArray(offset) < 0)
            None
          else
            Some(floydTemporaryArray(offset))
        }
      ).toMap
    ).toMap
  }
}
