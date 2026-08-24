package rummikub.server

import io.circe.syntax.*
import io.circe.parser.decode
import rummikub.model.*
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

class GameStateMessageSpec extends munit.FunSuite:

  test("a GameState server message round-trips through JSON") {
    val game    = Game.initial(List((PlayerId("p1"), "Alice"), (PlayerId("p2"), "Bob")))
    val message: ServerMessage = ServerMessage.GameState(GameViews.forPlayer(game, PlayerId("p1"), Set.empty))

    assertEquals(decode[ServerMessage](message.asJson.noSpaces), Right(message))
  }

  test("a GameStarted server message round-trips through JSON") {
    val message: ServerMessage = ServerMessage.GameStarted
    assertEquals(decode[ServerMessage](message.asJson.noSpaces), Right(message))
  }
