package com.amxl.generalsioai.models

object OfficialMessages {
  sealed trait MessageToServer
  sealed trait MessageFromServer

  case class StarsAndRank(userId: String) extends MessageToServer
  case class JoinPrivate(gameId: String, userId: String) extends MessageToServer
  case class SetUsername(userId: String, name: String) extends MessageToServer
  case class SetForceStart(userId: String, isForced: Boolean) extends MessageToServer
  case class Attack(source: Int, dest: Int, isHalf: Boolean, attackIndex: Int) extends MessageToServer

  case class QueueUpdate(memberCount: Int, forceCount: Int, timeout: Int) extends MessageFromServer
  case object PreGameStart extends MessageFromServer
  case class GameStart(playerIndex: Int, replayId: String, chatRoom: String, usernames: List[String]) extends MessageFromServer
  case class ScoreDetails(total: Int, tiles: Int, i: Int, dead: Boolean)
  case class GameUpdate(scores: List[ScoreDetails], turn: Int, attackIndex: Int, generals: List[Int],
                        mapDiff: List[Int], citiesDiff: List[Int]) extends MessageFromServer
  case object GameWon extends MessageFromServer
  case object GameLost extends MessageFromServer
  case class ChatMessage(chatRoom: String, text: String) extends MessageFromServer

  // Diff: <segments>
  // segment: <copyCount> <insertLength> <insertData> - length & data optional on last segment.
  // Map layout: <width>, <height>, <armyCounts>, <terrain>
  // Terrain: -1: unowned. -2: mountain, -3: Fog, -4 foggy mountain, 0..: team ID
  // Cities is just a list of city coordinates.


}
