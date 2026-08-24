package rummikub.protocol

import io.circe.syntax.*
import io.circe.parser.decode
import rummikub.model.PlayerId

class ProtocolCodecSpec extends munit.FunSuite:

  test("a ClientMessage round-trips through JSON") {
    val message: ClientMessage = ClientMessage.Join("Alice")
    assertEquals(decode[ClientMessage](message.asJson.noSpaces), Right(message))
  }

  test("a parameterless ClientMessage round-trips through JSON") {
    val message: ClientMessage = ClientMessage.Start
    assertEquals(decode[ClientMessage](message.asJson.noSpaces), Right(message))
  }

  test("a ServerMessage carrying lobby players round-trips through JSON") {
    val message: ServerMessage =
      ServerMessage.LobbyUpdated(List(LobbyPlayer(PlayerId("p1"), "Alice", isAi = false)))
    assertEquals(decode[ServerMessage](message.asJson.noSpaces), Right(message))
  }
