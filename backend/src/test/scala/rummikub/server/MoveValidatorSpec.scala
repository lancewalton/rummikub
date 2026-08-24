package rummikub.server

import cats.data.NonEmptyList
import rummikub.model.*
import rummikub.model.Piece.{Fixed, Joker}

class MoveValidatorSpec extends munit.FunSuite:

  private def red(n: Int): Fixed = Fixed(Colour.Red, n)
  private def run(pieces: Piece*): Group = Group.Run(NonEmptyList.fromListUnsafe(pieces.toList))
  private def set(pieces: Piece*): Group = Group.Number(NonEmptyList.fromListUnsafe(pieces.toList))

  private def player(id: String, firstMove: Boolean, rack: Piece*): Player =
    Player(PlayerId(id), id, Bag(rack.toList), firstMove)

  private def game(board: Board, current: Player, others: Player*): Game =
    val all = current +: others
    Game(board, Bag.empty, all.map(p => p.id -> p).toMap, all.map(_.id).toList, current.id, 0)

  private def validate(game: Game, proposed: Board): Either[String, Game] =
    MoveValidator(game, game.currentPlayerId, proposed)

  test("accepts a non-first move that extends a board run with a rack tile") {
    val g        = game(Board(List(run(red(3), red(4), red(5)))), player("p1", firstMove = false, red(6)), player("p2", firstMove = false))
    val proposed = Board(List(run(red(3), red(4), red(5), red(6))))

    val result = validate(g, proposed).getOrElse(fail("expected a valid move"))

    assertEquals(result.board, proposed)
    assert(result.players(PlayerId("p1")).rack.isEmpty)
    assertEquals(result.currentPlayerId, PlayerId("p2"))
  }

  test("rejects a proposed board containing an invalid group") {
    val g        = game(Board(Nil), player("p1", firstMove = false, red(3), red(4)))
    val proposed = Board(List(run(red(3), red(4))))

    assert(validate(g, proposed).isLeft)
  }

  test("rejects a move that removes a tile already on the board") {
    val g        = game(Board(List(run(red(3), red(4), red(5)))), player("p1", firstMove = false, red(6)))
    val proposed = Board(List(run(red(4), red(5), red(6))))

    assert(validate(g, proposed).isLeft)
  }

  test("rejects playing a tile the player does not hold") {
    val g        = game(Board(Nil), player("p1", firstMove = false, red(3)))
    val proposed = Board(List(run(red(3), red(4), red(5))))

    assert(validate(g, proposed).isLeft)
  }

  test("rejects a move that plays no tile from the rack") {
    val g        = game(Board(List(run(red(3), red(4), red(5)))), player("p1", firstMove = false, red(9)))
    val proposed = Board(List(run(red(3), red(4), red(5))))

    assert(validate(g, proposed).isLeft)
  }

  test("accepts a first move worth at least thirty") {
    val g        = game(Board(Nil), player("p1", firstMove = true, red(10), red(11), red(12)))
    val proposed = Board(List(run(red(10), red(11), red(12))))

    assert(validate(g, proposed).isRight)
  }

  test("rejects a first move worth less than thirty") {
    val g        = game(Board(Nil), player("p1", firstMove = true, red(1), Fixed(Colour.Blue, 1), Fixed(Colour.Black, 1)))
    val proposed = Board(List(set(red(1), Fixed(Colour.Blue, 1), Fixed(Colour.Black, 1))))

    assert(validate(g, proposed).isLeft)
  }

  test("rejects rearranging the existing board on a first move") {
    val g        = game(Board(List(run(red(1), red(2), red(3)), run(red(4), red(5), red(6)))), player("p1", firstMove = true, red(10), red(11), red(12)))
    val proposed = Board(List(run(red(1), red(2), red(3), red(4), red(5), red(6)), run(red(10), red(11), red(12))))

    assert(validate(g, proposed).isLeft)
  }
