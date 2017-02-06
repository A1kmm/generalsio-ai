package com.amxl.generalsioai.codecs
import com.amxl.generalsioai.models.OfficialMessages._
import io.circe._
import io.circe.parser.decode
import io.circe.generic.semiauto._

object OfficialMessageCodec {
  implicit val scoreDetailsDecoder : Decoder[ScoreDetails] = deriveDecoder[ScoreDetails]

  val messageFromServerDecoder : Decoder[MessageFromServer] = (cursor: HCursor) => for {
    cursors <- implicitly[Decoder[Array[HCursor]]].apply(cursor)
    cursorLookup = cursors.lift.andThen(r => r.toRight(DecodingFailure("Not enough data in array",
      cursors.lastOption.getOrElse(cursor).history)))
    messageType <- cursorLookup(0).flatMap(_.as[String])
    result <- messageType match {
      case "queue_update" => for {
        queueData <- cursorLookup(1)
        memberCount <- queueData.get[Int]("numPlayers")
        forceCount <- queueData.get[Int]("numForce")
      } yield (QueueUpdate(memberCount = memberCount, forceCount = forceCount))
      case "game_update" => for {
        gameData <- cursorLookup(1)
        scores <- gameData.get[List[ScoreDetails]]("scores")
        turn <- gameData.get[Int]("turn")
        generals <- gameData.get[List[Int]]("generals")
        mapDiff <- gameData.get[List[Int]]("map_diff")
        citiesDiff <- gameData.get[List[Int]]("cities_diff")
      } yield (GameUpdate(scores = scores, turn = turn, generals = generals,
                          mapDiff = mapDiff, citiesDiff = citiesDiff))
      case "pre_game_start" => Right(PreGameStart)
      case "game_won" => Right(GameWon)
      case "game_lost" => Right(GameLost)
      case "game_start" => for {
        gameCursor <- cursorLookup(1)
        playerIndex <- gameCursor.get[Int]("playerIndex")
        replayId <- gameCursor.get[String]("replay_id")
        chatRoom <- gameCursor.get[String]("chat_room")
        usernames <- gameCursor.get[List[String]]("usernames")
      } yield (GameStart(playerIndex = playerIndex, replayId = replayId, chatRoom = chatRoom, usernames = usernames))
      case "chat_message" => for {
        chatRoom <- cursorLookup(1).flatMap(_.as[String])
        body <- cursorLookup(2)
        text <- body.get[String]("text")
      } yield (ChatMessage(chatRoom = chatRoom, text = text))
      case _ => Left(DecodingFailure("Unknown message type " + messageType, cursor.history))
    }
  } yield (result)

  val messageToServerEncoder : Encoder[MessageToServer] = (messageToServer) => messageToServer match {
    case StarsAndRank(userId) => Json.arr(Encoder.encodeString("stars_and_rank"), Encoder.encodeString(userId))
    case JoinPrivate(gameId, userId) => Json.arr(
      Encoder.encodeString("join_private"),
      Encoder.encodeString(gameId),
      Encoder.encodeString(""),
      Encoder.encodeString(userId)
    )
    case Attack(source, dest, isHalf) => Json.arr(
      Encoder.encodeString("attack"),
      Encoder.encodeInt(source),
      Encoder.encodeInt(dest),
      Encoder.encodeBoolean(isHalf)
    )
    case SetForceStart(userId, isForced) => Json.arr(
      Encoder.encodeString("set_force_start"),
      Encoder.encodeString(userId),
      Encoder.encodeBoolean(isForced)
    )
    case SetUsername(username, name) => Json.arr(
      Encoder.encodeString("set_username"),
      Encoder.encodeString(username),
      Encoder.encodeString(name)
    )
    case LeaveGame() => Json.arr(Encoder.encodeString("leave_game"))
  }

  def parseMessageFromServer(input: String): Either[String, MessageFromServer] =
    decode(input)(messageFromServerDecoder).left.map(err => err.getMessage)

  def encodeMessageToServer(input: MessageToServer): String =
    messageToServerEncoder(input).noSpaces
}
