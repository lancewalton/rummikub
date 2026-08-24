package rummikub.frontend

import rummikub.model.{PlayerId, RoomCode}
import rummikub.protocol.*

enum Phase:
  case Joining, Lobby, InGame

final case class AppState(
    me: Option[PlayerId],
    roomCode: Option[RoomCode],
    lobby: List[LobbyPlayer],
    game: Option[GameStateView],
    notice: Option[String],
    outcome: Option[Option[PlayerView]]
):
  def phase: Phase =
    if game.isDefined then Phase.InGame
    else if roomCode.isDefined then Phase.Lobby
    else Phase.Joining

  def yourTurn: Boolean = game.exists(view => view.currentPlayer == view.you) && outcome.isEmpty

object AppState:
  val initial: AppState = AppState(None, None, Nil, None, None, None)

  def reduce(state: AppState, message: ServerMessage): AppState = message match
    case ServerMessage.Welcome(you)         => state.copy(me = Some(you))
    case ServerMessage.RoomJoined(code)     => state.copy(roomCode = Some(code), notice = None)
    case ServerMessage.RoomNotFound         => state.copy(notice = Some("No game found with that code."))
    case ServerMessage.LobbyUpdated(players) => state.copy(lobby = players)
    case ServerMessage.GameStarted          => state.copy(outcome = None)
    case ServerMessage.GameState(view) if state.me.contains(view.you) => state.copy(game = Some(view), notice = None)
    case ServerMessage.GameState(_)         => state
    case ServerMessage.MoveRejected(reason) => state.copy(notice = Some(reason))
    case ServerMessage.GameOver(winner)     => state.copy(outcome = Some(winner))
