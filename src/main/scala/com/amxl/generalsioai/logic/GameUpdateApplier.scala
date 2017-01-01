package com.amxl.generalsioai.logic

import com.amxl.generalsioai.models.State._
import com.amxl.generalsioai.models.OfficialMessages._

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

  val initialOfficialState = OfficialStateArrays(generals = List(), map = List(), cities = List())

  def updateOfficialState(update: GameUpdate, state: OfficialStateArrays): OfficialStateArrays =
    state.copy(generals = update.generals, map = applyDiff(update.mapDiff, state.map),
      cities = applyDiff(update.citiesDiff, state.cities))
}
