package com.amxl.generalsioai.codecs

import org.specs2.mutable.Specification
import com.amxl.generalsioai.models.OfficialMessages._

class OfficialMessageCodecSpec extends Specification {
  "OfficialMessageCodec should" >> {
    "Decode server to client messages" in {
      case class TestCase(input: String, expectedResult: MessageFromServer)
      val cases = Seq(
        TestCase(input = """["queue_update",{"numPlayers":1,"numForce":0}]""",
                 expectedResult = QueueUpdate(memberCount = 1, forceCount = 0)),
        TestCase(input = """["pre_game_start"]""",
                 expectedResult = PreGameStart),
        TestCase(input =
          """["game_start",
            | {
            |  "playerIndex":0,"replay_id":"replayidhere",
            |  "chat_room":"game_chatroomhere",
            |  "usernames":["PlayerA","PlayerB"],
            |  "teams":null
            | },
            | null
            |]""".stripMargin, expectedResult =
              GameStart(
                  playerIndex = 0,
                  replayId = "replayidhere",
                  chatRoom = "game_chatroomhere",
                  usernames = List("PlayerA", "PlayerB")
              )),
        TestCase(input =
          """
            |["game_update",
            | {
            |   "scores":[
            |     {"total":2,"tiles":1,"i":0,"dead":false},
            |     {"total":3,"tiles":4,"i":1,"dead":true}
            |   ],
            |   "turn":2, "attackIndex":0, "generals":[187,-1],
            |   "map_diff":[189,1,2,460], "cities_diff":[0]
            | },
            | null]
          """.stripMargin, expectedResult = GameUpdate(
            scores = List(ScoreDetails(total = 2, tiles = 1, i = 0, dead = false),
                           ScoreDetails(total = 3, tiles = 4, i = 1, dead = true)),
            turn = 2,
            generals = List(187, -1),
            mapDiff = List(189, 1, 2, 460),
            citiesDiff = List(0)
          )),
        TestCase(input = """["game_won"]""", expectedResult = GameWon),
        TestCase(input = """["game_lost"]""", expectedResult = GameLost),
        TestCase(input = """["chat_message","game_chatroom",{"text":"Anonymous wins!"}]""",
                 expectedResult = ChatMessage(chatRoom = "game_chatroom", text = "Anonymous wins!"))
      )

      cases map { testCase =>
        (OfficialMessageCodec.parseMessageFromServer(testCase.input) aka ("result of parsing " + testCase.input)
        ) must_== Right(testCase.expectedResult)
      }
    }

    "Encode client to server messages" in {
      case class TestCase(input: MessageToServer, expectedResult: String)
      val cases = Seq(
        TestCase(input = StarsAndRank(userId = "helloworld"), expectedResult = """["stars_and_rank","helloworld"]"""),
        TestCase(input = JoinPrivate(gameId = "game", userId = "user"), expectedResult = """["join_private","game","","user"]"""),
        TestCase(input = SetUsername(userId = "user", name = "bob"), expectedResult = """["set_username","user","bob"]"""),
        TestCase(input = SetForceStart(queueId = "queue", isForced = true), expectedResult = """["set_force_start","queue",true]"""),
        TestCase(input = Attack(source = 123, dest = 124, isHalf = false), expectedResult =
          """["attack",123,124,false]"""),
        TestCase(input = LeaveGame(), expectedResult = """["leave_game"]"""),
        TestCase(input = Join1v1("blah"), expectedResult = """["join_1v1","blah"]""")
      )

      cases map { testCase =>
        (OfficialMessageCodec.encodeMessageToServer(testCase.input) aka ("result of encoding " + testCase.input)
          ) must_== testCase.expectedResult
      }
    }
  }
}
