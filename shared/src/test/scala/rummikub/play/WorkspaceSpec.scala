package rummikub.play

import rummikub.model.Colour
import rummikub.protocol.TileView

class WorkspaceSpec extends munit.FunSuite:

  private def t(n: Int): TileView = TileView.NumberTile(Colour.Red, n)

  private def rackViews(ws: Workspace): List[List[TileView]] = ws.rackRows.map(_.tiles.map(_.view))
  private def boardViews(ws: Workspace): List[List[TileView]] = ws.boardRows.map(_.tiles.map(_.view))

  test("fromBoardAndRack keeps board groups as board rows and all rack tiles in one rack row") {
    val ws = Workspace.fromBoardAndRack(List(List(t(3), t(4), t(5))), List(t(8), t(9)))
    assertEquals(boardViews(ws), List(List(t(3), t(4), t(5))))
    assertEquals(rackViews(ws), List(List(t(8), t(9))))
  }

  test("moving a tile to a new rack row splits it off into its own row") {
    val ws     = Workspace.fromBoardAndRack(Nil, List(t(1), t(2)))
    val firstId = ws.rackRows.head.tiles.head.id
    val moved  = ws.move(firstId, DropTarget.NewRow(Zone.Rack))
    assertEquals(rackViews(moved), List(List(t(2)), List(t(1))))
  }

  test("inserting a tile at a column index reorders within the row") {
    val ws    = Workspace.fromBoardAndRack(Nil, List(t(3), t(5)))
    val rowId = ws.rackRows.head.id
    val fiveId = ws.rackRows.head.tiles(1).id
    val moved = ws.move(fiveId, DropTarget.IntoRow(rowId, 0))
    assertEquals(rackViews(moved), List(List(t(5), t(3))))
  }

  test("inserting a tile at a later column index in the same row adjusts for its own removal") {
    val ws    = Workspace.fromBoardAndRack(Nil, List(t(3), t(4), t(5)))
    val rowId = ws.rackRows.head.id
    val threeId = ws.rackRows.head.tiles(0).id
    val moved = ws.move(threeId, DropTarget.IntoRow(rowId, 2))
    assertEquals(rackViews(moved), List(List(t(4), t(3), t(5))))
  }

  test("moving a tile into another row removes it from the source and inserts it into the target") {
    val ws       = Workspace.fromBoardAndRack(List(List(t(6), t(7))), List(t(8), t(9)))
    val boardRowId = ws.boardRows.head.id
    val eightId  = ws.rackRows.head.tiles.head.id
    val moved    = ws.move(eightId, DropTarget.IntoRow(boardRowId, 2))
    assertEquals(boardViews(moved), List(List(t(6), t(7), t(8))))
    assertEquals(rackViews(moved), List(List(t(9))))
  }

  test("emptying a row prunes it") {
    val ws     = Workspace.fromBoardAndRack(Nil, List(t(1)))
    val onlyId = ws.rackRows.head.tiles.head.id
    val moved  = ws.move(onlyId, DropTarget.NewRow(Zone.Rack))
    assertEquals(moved.rackRows.size, 1)
    assertEquals(rackViews(moved), List(List(t(1))))
  }

  test("boardGroups reflects only the board rows, in order") {
    val ws = Workspace.fromBoardAndRack(List(List(t(3), t(4), t(5)), List(t(9), t(9), t(9))), List(t(1)))
    assertEquals(ws.boardGroups, List(List(t(3), t(4), t(5)), List(t(9), t(9), t(9))))
  }
