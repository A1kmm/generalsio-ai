package com.amxl.generalsioai.models

import com.amxl.generalsioai.models.State.Coordinate

sealed trait ProposedAction
case object DoNothingAction extends ProposedAction
case class ProposedAttackAction(source: Coordinate, destination: Coordinate, halfStrength: Boolean) extends ProposedAction

sealed trait ProposedStrategy
case class TakeCellAt(position: Coordinate) extends ProposedStrategy
