package rummikub.server

import cats.effect.*
import cats.effect.std.{Mutex, Queue}
import cats.syntax.all.*
import rummikub.ai.AI
import rummikub.model.{Board, Game, PlayerId}
import rummikub.protocol.*

import java.util.UUID

// A single, isolated game: its own lobby, board, AI seats and connections.
final class GameRoom(
    lobby: Ref[IO, LobbyState],
    game: Ref[IO, Option[Game]],
    aiIds: Ref[IO, Set[PlayerId]],
    connections: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]],
    turnMutex: Mutex[IO]
):
  def join(myId: PlayerId, queue: Queue[IO, ServerMessage], name: String): IO[Unit] =
    connections.update(_ + (myId -> queue)) *> lobby.update(_.join(myId, name)) *> broadcastLobby

  def disconnect(myId: PlayerId): IO[Unit] =
    connections.update(_ - myId) *> lobby.update(_.remove(myId)) *> broadcastLobby

  def isEmpty: IO[Boolean] = connections.get.map(_.isEmpty)

  def handle(myId: PlayerId, message: ClientMessage): IO[Unit] = message match
    case ClientMessage.AddAi(name)        => newId.flatMap(id => lobby.update(_.addAi(id, name))) *> broadcastLobby
    case ClientMessage.Start              => start
    case ClientMessage.SubmitMove(groups) => submitMove(myId, GameViews.parseMove(groups))
    case ClientMessage.Draw               => draw(myId)
    case ClientMessage.PlayAgain          => playAgain
    case _: ClientMessage.CreateRoom | _: ClientMessage.JoinRoom => IO.unit

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

  private def submitMove(myId: PlayerId, proposed: Board): IO[Unit] =
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

  private def playAgain: IO[Unit] =
    turnMutex.lock.surround {
      game.get.flatMap {
        case Some(finished) if finished.isFinished =>
          val restarted = Rematch(finished)
          game.set(restarted.some) *> broadcast(ServerMessage.GameStarted) *> broadcastState(restarted) *> progress
        case _ => IO.unit
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

object GameRoom:
  def create: IO[GameRoom] =
    for
      lobby       <- Ref.of[IO, LobbyState](LobbyState.empty)
      game        <- Ref.of[IO, Option[Game]](None)
      aiIds       <- Ref.of[IO, Set[PlayerId]](Set.empty)
      connections <- Ref.of[IO, Map[PlayerId, Queue[IO, ServerMessage]]](Map.empty)
      turnMutex   <- Mutex[IO]
    yield GameRoom(lobby, game, aiIds, connections, turnMutex)
