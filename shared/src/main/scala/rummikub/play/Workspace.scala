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

  def canCommit: Boolean = boardRows.nonEmpty && boardRows.forall(rowIsValid)

  def rowIsValid(row: Row): Boolean = Grouping.isValidGroup(row.tiles.map(_.view))

  // Restore the board to the committed server groups, returning any tiles the
  // player had moved onto the board to a fresh rack row. Rack organisation is
  // left untouched.
  def resetBoard(serverGroups: List[List[TileView]]): Workspace =
    val returned            = removeMatching(boardRows.flatMap(_.tiles), serverGroups.flatten)
    val (freshBoard, nextR) = Workspace.numberRows(serverGroups, Zone.Board, maxRowId + 1, maxTileId + 1)
    val returnedRows        = if returned.isEmpty then Nil else List(Row(nextR, Zone.Rack, returned))
    val newRows             = freshBoard ++ rackRows ++ returnedRows
    Workspace(newRows, newRows.map(_.id).maxOption.getOrElse(-1) + 1)

  // Adopt a new server state (fresh board, authoritative rack) while keeping the
  // player's rack arrangement: tiles still in hand stay where they are, committed
  // tiles drop out, and newly drawn tiles arrive in a new row.
  def syncTo(serverGroups: List[List[TileView]], serverRack: List[TileView]): Workspace =
    val (freshBoard, nextRowId) = Workspace.numberRows(serverGroups, Zone.Board, maxRowId + 1, maxTileId + 1)
    val (keptRackRows, drawn)   = reconcileRack(serverRack)
    val drawnRow                = drawnRowOf(drawn, nextRowId, maxTileId + 1 + serverGroups.map(_.size).sum)
    val newRows                 = freshBoard ++ keptRackRows ++ drawnRow
    Workspace(newRows, newRows.map(_.id).maxOption.getOrElse(-1) + 1)

  private def reconcileRack(serverRack: List[TileView]): (List[Row], List[TileView]) =
    val available = serverRack.groupBy(identity).view.mapValues(_.size).toMap
    val (keptRows, remaining) =
      rackRows.foldLeft((List.empty[Row], available)) { case ((acc, avail), row) =>
        val (kept, nextAvail) = keepAvailableTiles(row.tiles, avail)
        (acc :+ row.copy(tiles = kept), nextAvail)
      }
    (keptRows.filter(_.tiles.nonEmpty), remaining.toList.flatMap((view, count) => List.fill(count)(view)))

  private def keepAvailableTiles(tiles: List[Tile], available: Map[TileView, Int]): (List[Tile], Map[TileView, Int]) =
    tiles.foldLeft((List.empty[Tile], available)) { case ((kept, avail), tile) =>
      if avail.getOrElse(tile.view, 0) > 0 then (kept :+ tile, avail.updated(tile.view, avail(tile.view) - 1))
      else (kept, avail)
    }

  private def drawnRowOf(drawn: List[TileView], rowId: Int, startTileId: Int): List[Row] =
    if drawn.isEmpty then Nil
    else List(Row(rowId, Zone.Rack, drawn.zipWithIndex.map((view, i) => Tile(startTileId + i, view))))

  private def maxRowId: Int  = rows.map(_.id).maxOption.getOrElse(-1)
  private def maxTileId: Int = rows.flatMap(_.tiles).map(_.id).maxOption.getOrElse(-1)

  private def removeMatching(tiles: List[Tile], views: List[TileView]): List[Tile] =
    views.foldLeft(tiles) { (remaining, view) =>
      remaining.indexWhere(_.view == view) match
        case -1 => remaining
        case i  => remaining.patch(i, Nil, 1)
    }

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

  // Build rows for the given groups, assigning row ids from startRowId and tile
  // ids from startTileId; returns the rows and the next free row id.
  private def numberRows(groups: List[List[TileView]], zone: Zone, startRowId: Int, startTileId: Int): (List[Row], Int) =
    val (rows, nextRowId, _) =
      groups.foldLeft((List.empty[Row], startRowId, startTileId)) { case ((acc, rowId, tileId), views) =>
        val (tiles, nextTileId) = numberTiles(views, tileId)
        (acc :+ Row(rowId, zone, tiles), rowId + 1, nextTileId)
      }
    (rows, nextRowId)
