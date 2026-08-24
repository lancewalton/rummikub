package rummikub.protocol

import io.circe.syntax.*
import io.circe.parser.decode
import rummikub.model.{PlayerId, RoomCode}

class ProtocolCodecSpec extends munit.FunSuite:

  test("a room-creation ClientMessage round-trips through JSON") {
    val message: ClientMessage = ClientMessage.CreateRoom("Alice")
    assertEquals(decode[ClientMessage](message.asJson.noSpaces), Right(message))
  }

  test("a room-join ClientMessage round-trips through JSON") {
    val message: ClientMessage = ClientMessage.JoinRoom(RoomCode("WXYZ"), "Bob")
    assertEquals(decode[ClientMessage](message.asJson.noSpaces), Right(message))
  }

  test("a parameterless ClientMessage round-trips through JSON") {
    val message: ClientMessage = ClientMessage.Start
    assertEquals(decode[ClientMessage](message.asJson.noSpaces), Right(message))
  }

  test("the play-again ClientMessage round-trips through JSON") {
    val message: ClientMessage = ClientMessage.PlayAgain
    assertEquals(decode[ClientMessage](message.asJson.noSpaces), Right(message))
  }

  test("a ServerMessage carrying lobby players round-trips through JSON") {
    val message: ServerMessage =
      ServerMessage.LobbyUpdated(List(LobbyPlayer(PlayerId("p1"), "Alice", isAi = false)))
    assertEquals(decode[ServerMessage](message.asJson.noSpaces), Right(message))
  }
