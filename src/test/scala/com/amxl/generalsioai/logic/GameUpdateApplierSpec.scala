package com.amxl.generalsioai.logic

import org.specs2.mutable.Specification
import GameUpdateApplier._
import com.amxl.generalsioai.models.OfficialMessages.GameUpdate

class GameUpdateApplierSpec extends Specification {
  private val blankUpdate = GameUpdate(scores = List(), turn = 0, attackIndex = 0, generals = List(),
    mapDiff = List(), citiesDiff = List())
  "Should apply generals" >> {
    updateOfficialState(blankUpdate.copy(generals = List(1, 2, 3)), initialOfficialState).generals must
      be_==(List(1, 2, 3))
  }

  "Should set up initial map state" >> {
    updateOfficialState(blankUpdate.copy(mapDiff = List(0, 5, 3, 5, 7, 11, 13)), initialOfficialState).map must
        be_==(List(3, 5, 7, 11, 13))
  }

  "Should update map state" >> {
    updateOfficialState(
      blankUpdate.copy(mapDiff = List(3, 1, 12, 1)),
      initialOfficialState.copy(map = List(3, 5, 7, 11, 13))).map must be_==(List(3, 5, 7, 12, 13))
  }

  "Should set up initial cities state" >> {
    updateOfficialState(blankUpdate.copy(citiesDiff = List(0, 5, 3, 5, 7, 11, 13)), initialOfficialState).cities must
        be_==(List(3, 5, 7, 11, 13))
  }

  "Should update cities state" >> {
    updateOfficialState(
      blankUpdate.copy(citiesDiff = List(3, 1, 12, 1)),
      initialOfficialState.copy(cities = List(3, 5, 7, 11, 13))).cities must be_==(List(3, 5, 7, 12, 13))
  }
}
