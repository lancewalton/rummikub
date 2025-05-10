package rumikub.ai

import rumikub.model.*

object ValidGroupFinder {
  def apply(boardAndPlayer: BoardAndPlayer): Set[Group] = {
    println("Finding valid groups")
    apply(boardAndPlayer.transferAllToPlayer.player)
  }

  private def apply(pieces: Bag) = SequenceGroups(pieces) ++ NumberGroups(pieces)
}
