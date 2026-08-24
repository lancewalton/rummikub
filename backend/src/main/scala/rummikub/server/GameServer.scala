package rummikub.server

import cats.effect.*
import cats.effect.std.{Mutex, Queue}
import cats.syntax.all.*
import fs2.Stream
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.Response
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import rummikub.ai.AI
import rummikub.model.{Game, PlayerId}
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

import java.util.UUID

final class GameServer(
    lobby: Ref[IO, LobbyState],
    game: Ref[IO, Option[Game]],
    aiIds: Ref[IO, Set[PlayerId]],
    connections: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]],
    turnMutex: Mutex[IO]
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
    case ClientMessage.Join(name)       => lobby.update(_.join(myId, name)) *> broadcastLobby
    case ClientMessage.AddAi(name)      => newId.flatMap(id => lobby.update(_.addAi(id, name))) *> broadcastLobby
    case ClientMessage.Start            => start
    case ClientMessage.SubmitMove(groups) => submitMove(myId, GameViews.parseMove(groups))
    case ClientMessage.Draw             => draw(myId)

  private def start: IO[Unit] =
    turnMutex.lock.surround {
      lobby.get.flatMap { state =>
        state.startGame.fold(IO.unit) { started =>
          val ais = state.members.filter(_.isAi).map(_.id).toSet
          game.set(started.some) *> aiIds.set(ais) *> lobby.set(LobbyState.empty) *>
            broadcast(ServerMessage.GameStarted) *> broadcastState(started) *> progress
        }
      }
    }

  private def submitMove(myId: PlayerId, proposed: rummikub.model.Board): IO[Unit] =
    turnMutex.lock.surround {
      onTurn(myId) { current =>
        MoveValidator(current, myId, proposed).fold(
          reason => sendTo(myId, ServerMessage.MoveRejected(reason)),
          updated => game.set(updated.some) *> broadcastState(updated) *> progress
        )
      }
    }

  private def draw(myId: PlayerId): IO[Unit] =
    turnMutex.lock.surround {
      onTurn(myId) { current =>
        val updated = current.noPlayAvailableForCurrentPlayer
        game.set(updated.some) *> broadcastState(updated) *> progress
      }
    }

  private def onTurn(myId: PlayerId)(action: Game => IO[Unit]): IO[Unit] =
    game.get.flatMap {
      case Some(current) if current.currentPlayerId == myId => action(current)
      case Some(_)                                          => sendTo(myId, ServerMessage.MoveRejected("It is not your turn"))
      case None                                             => IO.unit
    }

  private def progress: IO[Unit] =
    for
      maybeGame <- game.get
      ais       <- aiIds.get
      _         <- maybeGame.fold(IO.unit)(advance(_, ais))
    yield ()

  private def advance(current: Game, ais: Set[PlayerId]): IO[Unit] =
    if current.isFinished then broadcastGameOver(current)
    else if ais.contains(current.currentPlayerId) then
      val next = aiMove(current)
      game.set(next.some) *> broadcastState(next) *> progress
    else IO.unit

  private def aiMove(current: Game): Game =
    val player = current.currentPlayer
    AI(current.board, player).fold(current.noPlayAvailableForCurrentPlayer) { move =>
      current.update(move.board, player.copy(rack = move.player))
    }

  private def disconnect(myId: PlayerId): IO[Unit] =
    connections.update(_ - myId) *> lobby.update(_.remove(myId)) *> broadcastLobby

  private def broadcastLobby: IO[Unit] =
    lobby.get.flatMap(state => broadcast(ServerMessage.LobbyUpdated(state.toLobbyPlayers)))

  private def broadcastState(current: Game): IO[Unit] =
    aiIds.get.flatMap(ais => current.playerSequence.traverse_(id => sendTo(id, ServerMessage.GameState(GameViews.forPlayer(current, id, ais)))))

  private def broadcastGameOver(current: Game): IO[Unit] =
    aiIds.get.flatMap { ais =>
      val winner = current.players.values.find(_.rack.isEmpty).map(GameViews.playerView(_, ais))
      broadcast(ServerMessage.GameOver(winner))
    }

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
      aiIds       <- Ref.of[IO, Set[PlayerId]](Set.empty)
      connections <- Ref.of[IO, Map[PlayerId, Queue[IO, ServerMessage]]](Map.empty)
      turnMutex   <- Mutex[IO]
    yield GameServer(lobby, game, aiIds, connections, turnMutex)
