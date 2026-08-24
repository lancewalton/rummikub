package rummikub.server

import cats.data.NonEmptyList
import rummikub.model.*
import rummikub.model.Piece.{Fixed, Joker}
import rummikub.protocol.*

class GameViewsSpec extends munit.FunSuite:

  private def red(n: Int): Fixed = Fixed(Colour.Red, n)

  test("a joker becomes a joker tile and a fixed piece becomes a number tile") {
    assertEquals(GameViews.tile(Joker), TileView.JokerTile)
    assertEquals(GameViews.tile(red(5)), TileView.NumberTile(Colour.Red, 5))
  }

  test("a board's runs and sets become group views with the right kind and tiles") {
    val board = Board(List(
      Group.Run(NonEmptyList.of(red(3), red(4), red(5))),
      Group.Number(NonEmptyList.of(red(7), Fixed(Colour.Blue, 7), Fixed(Colour.Black, 7)))
    ))

    val view = GameViews.board(board)

    assertEquals(view.groups.map(_.kind), List(GroupKind.Run, GroupKind.Set))
    assertEquals(view.groups.head.tiles, List(TileView.NumberTile(Colour.Red, 3), TileView.NumberTile(Colour.Red, 4), TileView.NumberTile(Colour.Red, 5)))
  }

  test("a per-player view exposes that player's rack, all players' counts and the current player") {
    val game = Game.initial(List((PlayerId("p1"), "Alice"), (PlayerId("p2"), "Bob")))

    val view = GameViews.forPlayer(game, PlayerId("p1"))

    assertEquals(view.you, PlayerId("p1"))
    assertEquals(view.yourTiles.size, 14)
    assertEquals(view.players.map(_.id).toSet, Set(PlayerId("p1"), PlayerId("p2")))
    assert(view.players.forall(_.tileCount == 14))
    assertEquals(view.currentPlayer, game.currentPlayerId)
    assert(view.board.groups.isEmpty)
  }
