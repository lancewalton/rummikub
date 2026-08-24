package rummikub.server

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.Response
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import rummikub.model.{Game, PlayerId}
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

import java.util.UUID

final class GameServer(
    lobby: Ref[IO, LobbyState],
    game: Ref[IO, Option[Game]],
    connections: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]]
):
  def webSocket(wsb: WebSocketBuilder2[IO]): IO[Response[IO]] =
    for
      myId  <- newId
      queue <- Queue.unbounded[IO, ServerMessage]
      _     <- connections.update(_ + (myId -> queue))
      _     <- queue.offer(ServerMessage.Welcome(myId))
      _     <- lobby.get.flatMap(state => queue.offer(ServerMessage.LobbyUpdated(state.toLobbyPlayers)))
      response <- wsb.build(outbound(queue), inbound(myId))
    yield response

  private def outbound(queue: Queue[IO, ServerMessage]): Stream[IO, WebSocketFrame] =
    Stream.fromQueueUnterminated(queue).map(message => WebSocketFrame.Text(message.asJson.noSpaces))

  private def inbound(myId: PlayerId): fs2.Pipe[IO, WebSocketFrame, Unit] =
    _.collect { case WebSocketFrame.Text(text, _) => text }
      .evalMap(text => decode[ClientMessage](text).fold(_ => IO.unit, handle(myId, _)))
      .onFinalize(disconnect(myId))

  private def handle(myId: PlayerId, message: ClientMessage): IO[Unit] = message match
    case ClientMessage.Join(name)  => lobby.update(_.join(myId, name)) *> broadcastLobby
    case ClientMessage.AddAi(name) => newId.flatMap(id => lobby.update(_.addAi(id, name))) *> broadcastLobby
    case ClientMessage.Start       => start

  private def start: IO[Unit] =
    lobby.get.flatMap { state =>
      state.startGame.fold(IO.unit) { started =>
        game.set(started.some) *> broadcast(ServerMessage.GameStarted) *> sendGameState(started)
      }
    }

  private def sendGameState(started: Game): IO[Unit] =
    started.playerSequence.traverse_(id => sendTo(id, ServerMessage.GameState(GameViews.forPlayer(started, id))))

  private def disconnect(myId: PlayerId): IO[Unit] =
    connections.update(_ - myId) *> lobby.update(_.remove(myId)) *> broadcastLobby

  private def broadcastLobby: IO[Unit] =
    lobby.get.flatMap(state => broadcast(ServerMessage.LobbyUpdated(state.toLobbyPlayers)))

  private def broadcast(message: ServerMessage): IO[Unit] =
    connections.get.flatMap(_.values.toList.traverse_(_.offer(message)))

  private def sendTo(playerId: PlayerId, message: ServerMessage): IO[Unit] =
    connections.get.flatMap(_.get(playerId).traverse_(_.offer(message)))

  private def newId: IO[PlayerId] = IO(PlayerId(UUID.randomUUID().toString))

object GameServer:
  def create: IO[GameServer] =
    for
      lobby       <- Ref.of[IO, LobbyState](LobbyState.empty)
      game        <- Ref.of[IO, Option[Game]](None)
      connections <- Ref.of[IO, Map[PlayerId, Queue[IO, ServerMessage]]](Map.empty)
    yield GameServer(lobby, game, connections)
