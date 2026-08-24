package rummikub.play

import cats.data.NonEmptyList
import rummikub.model.*
import rummikub.model.Piece.{Fixed, Joker}
import rummikub.protocol.TileView

object Grouping:
  def toPiece(tile: TileView): Piece = tile match
    case TileView.JokerTile                  => Joker
    case TileView.NumberTile(colour, number) => Fixed(colour, number)

  def interpret(tiles: List[TileView]): Option[Group] =
    NonEmptyList.fromList(tiles.map(toPiece)).map { pieces =>
      val asRun = Group.Run(pieces)
      if asRun.isValid then asRun else Group.Number(pieces)
    }

  def isValidGroup(tiles: List[TileView]): Boolean = interpret(tiles).exists(_.isValid)
