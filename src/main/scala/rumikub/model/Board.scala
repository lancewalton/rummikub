package rumikub.model

import scala.annotation.tailrec

case class Board(groups: List[Group]) {
  def pieces: Bag = Bag(
    groups.flatMap {
      case Group.Run(s) => s.toList
      case Group.Number(s) => s.toList
    }
  )

  def +(group: Group): Board = Board(group :: groups)
  def -(group: Group): Board = Board(removeFirstMatch(group, groups))

  def contains(group: Group): Boolean = groups.contains(group)
}

object Board {
  val empty: Board = Board(Nil)
}