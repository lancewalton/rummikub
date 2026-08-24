package rummikub.play

import rummikub.model.Colour
import rummikub.model.Group
import rummikub.protocol.TileView

class GroupingSpec extends munit.FunSuite:

  private def red(n: Int): TileView    = TileView.NumberTile(Colour.Red, n)
  private def tile(c: Colour, n: Int): TileView = TileView.NumberTile(c, n)

  test("a consecutive same-colour run is a valid group") {
    assert(Grouping.isValidGroup(List(red(3), red(4), red(5))))
  }

  test("a same-number distinct-colour set is a valid group") {
    assert(Grouping.isValidGroup(List(red(7), tile(Colour.Blue, 7), tile(Colour.Black, 7))))
  }

  test("a joker filling a run is a valid group") {
    assert(Grouping.isValidGroup(List(red(3), TileView.JokerTile, red(5))))
  }

  test("a mixed-up trio is not a valid group") {
    assert(!Grouping.isValidGroup(List(red(3), tile(Colour.Blue, 4), red(5))))
  }

  test("a two-tile group is too short to be valid") {
    assert(!Grouping.isValidGroup(List(red(3), red(4))))
  }

  test("an empty group is not valid") {
    assert(!Grouping.isValidGroup(Nil))
  }

  test("interpret reads consecutive same-colour tiles as a run and same-number tiles as a set") {
    assert(Grouping.interpret(List(red(3), red(4), red(5))).exists(_.isInstanceOf[Group.Run]))
    assert(Grouping.interpret(List(red(7), tile(Colour.Blue, 7), tile(Colour.Black, 7))).exists(_.isInstanceOf[Group.Number]))
  }
