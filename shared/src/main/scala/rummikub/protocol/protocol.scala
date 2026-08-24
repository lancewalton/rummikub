package rummikub.protocol

import io.circe.{Codec, Decoder, Encoder}
import rummikub.model.{Colour, PlayerId}

object Codecs:
  given Codec[PlayerId] = Codec.from(
    Decoder.decodeString.map(PlayerId(_)),
    Encoder.encodeString.contramap(_.value)
  )

  given Codec[Colour] = Codec.from(
    Decoder.decodeString.emap(name => Colour.values.find(_.toString == name).toRight(s"Unknown colour: $name")),
    Encoder.encodeString.contramap(_.toString)
  )

import Codecs.given

final case class LobbyPlayer(id: PlayerId, name: String, isAi: Boolean) derives Codec.AsObject

enum TileView derives Codec.AsObject:
  case JokerTile
  case NumberTile(colour: Colour, number: Int)

enum GroupKind derives Codec.AsObject:
  case Run, Set

final case class GroupView(kind: GroupKind, tiles: List[TileView]) derives Codec.AsObject

final case class BoardView(groups: List[GroupView]) derives Codec.AsObject

final case class PlayerView(id: PlayerId, name: String, isAi: Boolean, tileCount: Int) derives Codec.AsObject

final case class GameStateView(
    you: PlayerId,
    yourTiles: List[TileView],
    board: BoardView,
    players: List[PlayerView],
    currentPlayer: PlayerId
) derives Codec.AsObject

enum ClientMessage derives Codec.AsObject:
  case Join(playerName: String)
  case AddAi(name: String)
  case Start
  case SubmitMove(groups: List[List[TileView]])
  case Draw

enum ServerMessage derives Codec.AsObject:
  case Welcome(you: PlayerId)
  case LobbyUpdated(players: List[LobbyPlayer])
  case GameStarted
  case GameState(view: GameStateView)
  case MoveRejected(reason: String)
  case GameOver(winner: Option[PlayerView])
