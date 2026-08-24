package rummikub.frontend

import rummikub.protocol.TileView

final case class Tile(id: Int, view: TileView)

final case class StagedGroup(id: Int, tiles: List[Tile])

enum DropTarget:
  case ToGroup(groupId: Int)
  case NewGroup
  case ToRack

final case class Workspace(groups: List[StagedGroup], rack: List[Tile], nextGroupId: Int):
  def move(tileId: Int, target: DropTarget): Workspace =
    findTile(tileId).fold(this)(tile => removeTile(tileId).place(tile, target))

  def toGroups: List[List[TileView]] = groups.map(_.tiles.map(_.view))

  private def findTile(tileId: Int): Option[Tile] =
    (rack ++ groups.flatMap(_.tiles)).find(_.id == tileId)

  private def removeTile(tileId: Int): Workspace =
    copy(
      rack = rack.filterNot(_.id == tileId),
      groups = groups.map(group => group.copy(tiles = group.tiles.filterNot(_.id == tileId))).filter(_.tiles.nonEmpty)
    )

  private def place(tile: Tile, target: DropTarget): Workspace = target match
    case DropTarget.ToRack           => copy(rack = rack :+ tile)
    case DropTarget.NewGroup         => addGroup(tile)
    case DropTarget.ToGroup(groupId) =>
      if groups.exists(_.id == groupId) then appendToGroup(tile, groupId) else addGroup(tile)

  private def addGroup(tile: Tile): Workspace =
    copy(groups = groups :+ StagedGroup(nextGroupId, List(tile)), nextGroupId = nextGroupId + 1)

  private def appendToGroup(tile: Tile, groupId: Int): Workspace =
    copy(groups = groups.map(group => if group.id == groupId then group.copy(tiles = group.tiles :+ tile) else group))

object Workspace:
  def fromBoardAndRack(boardGroups: List[List[TileView]], rackTiles: List[TileView]): Workspace =
    val (groups, afterGroups) =
      boardGroups.foldLeft((List.empty[StagedGroup], 0)) { case ((acc, nextId), views) =>
        val (tiles, newNext) = numberTiles(views, nextId)
        (acc :+ StagedGroup(acc.size, tiles), newNext)
      }
    val (rack, _) = numberTiles(rackTiles, afterGroups)
    Workspace(groups, rack, groups.size)

  private def numberTiles(views: List[TileView], startId: Int): (List[Tile], Int) =
    (views.zipWithIndex.map((view, i) => Tile(startId + i, view)), startId + views.size)
