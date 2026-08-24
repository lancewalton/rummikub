package rummikub.play

import rummikub.protocol.TileView

enum Zone:
  case Board, Rack

final case class Tile(id: Int, view: TileView)

final case class Row(id: Int, zone: Zone, tiles: List[Tile])

enum DropTarget:
  case IntoRow(rowId: Int, index: Int)
  case NewRow(zone: Zone)

final case class Workspace(rows: List[Row], nextRowId: Int):
  def boardRows: List[Row] = rows.filter(_.zone == Zone.Board)
  def rackRows: List[Row]  = rows.filter(_.zone == Zone.Rack)

  def boardGroups: List[List[TileView]] = boardRows.map(_.tiles.map(_.view))

  def move(tileId: Int, target: DropTarget): Workspace =
    findTile(tileId).fold(this) { tile =>
      target match
        case DropTarget.NewRow(zone)       => addRow(tileId, zone, tile)
        case DropTarget.IntoRow(rowId, at) => insertInto(tileId, tile, rowId, at)
    }

  private def findTile(tileId: Int): Option[Tile] =
    rows.flatMap(_.tiles).find(_.id == tileId)

  private def addRow(tileId: Int, zone: Zone, tile: Tile): Workspace =
    Workspace(withoutTile(tileId) :+ Row(nextRowId, zone, List(tile)), nextRowId + 1).pruned

  private def insertInto(tileId: Int, tile: Tile, rowId: Int, index: Int): Workspace =
    copy(rows = rows.map(row => if row.id == rowId then insertTile(row, tileId, tile, index) else removeFrom(row, tileId))).pruned

  private def insertTile(row: Row, tileId: Int, tile: Tile, index: Int): Row =
    val without      = row.tiles.filterNot(_.id == tileId)
    val currentIndex = row.tiles.indexWhere(_.id == tileId)
    val adjusted     = if currentIndex >= 0 && currentIndex < index then index - 1 else index
    val clamped      = adjusted.max(0).min(without.size)
    row.copy(tiles = (without.take(clamped) :+ tile) ++ without.drop(clamped))

  private def removeFrom(row: Row, tileId: Int): Row =
    row.copy(tiles = row.tiles.filterNot(_.id == tileId))

  private def withoutTile(tileId: Int): List[Row] =
    rows.map(removeFrom(_, tileId))

  private def pruned: Workspace = copy(rows = rows.filter(_.tiles.nonEmpty))

object Workspace:
  def fromBoardAndRack(boardGroups: List[List[TileView]], rackTiles: List[TileView]): Workspace =
    val zonedGroups = boardGroups.map(Zone.Board -> _) ++ Option.when(rackTiles.nonEmpty)(Zone.Rack -> rackTiles)
    val (rows, _) = zonedGroups.foldLeft((List.empty[Row], 0)) { case ((acc, nextTileId), (zone, views)) =>
      val (tiles, newNext) = numberTiles(views, nextTileId)
      (acc :+ Row(acc.size, zone, tiles), newNext)
    }
    Workspace(rows, rows.size)

  private def numberTiles(views: List[TileView], startId: Int): (List[Tile], Int) =
    (views.zipWithIndex.map((view, i) => Tile(startId + i, view)), startId + views.size)
