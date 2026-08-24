package rummikub.server

import rummikub.model.*
import rummikub.model.Piece.{Fixed, Joker}
import rummikub.protocol.*

object GameViews:
  def tile(piece: Piece): TileView = piece match
    case Joker           => TileView.JokerTile
    case Fixed(colour, n) => TileView.NumberTile(colour, n)

  def board(board: Board): BoardView =
    BoardView(board.groups.map(group))

  def parseMove(groups: List[List[TileView]]): Board =
    Board(groups.flatMap(parseGroup))

  private def parseGroup(tiles: List[TileView]): Option[Group] =
    cats.data.NonEmptyList.fromList(tiles.map(toPiece)).map { pieces =>
      val asRun = Group.Run(pieces)
      if asRun.isValid then asRun else Group.Number(pieces)
    }

  private def toPiece(tile: TileView): Piece = tile match
    case TileView.JokerTile          => Joker
    case TileView.NumberTile(colour, number) => Fixed(colour, number)

  def forPlayer(game: Game, playerId: PlayerId, aiIds: Set[PlayerId]): GameStateView =
    GameStateView(
      you = playerId,
      yourTiles = tilesOf(game.players(playerId).rack),
      board = board(game.board),
      players = game.playerSequence.map(id => playerView(game.players(id), aiIds)),
      currentPlayer = game.currentPlayerId
    )

  private def group(group: Group): GroupView =
    GroupView(kindOf(group), group.pieces.toList.map(tile))

  private def kindOf(group: Group): GroupKind = group match
    case _: Group.Run    => GroupKind.Run
    case _: Group.Number => GroupKind.Set

  def playerView(player: Player, aiIds: Set[PlayerId]): PlayerView =
    PlayerView(player.id, player.name, isAi = aiIds.contains(player.id), tileCount = tilesOf(player.rack).size)

  private def tilesOf(bag: Bag): List[TileView] = bag.piecesAsVector.toList.map(tile)
