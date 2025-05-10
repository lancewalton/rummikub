package rumikub.ai

import rumikub.model.*

import scala.annotation.tailrec

object BoardDiffScorer {
  def apply(from: Board, to: Board): Int = countNonIdentical(from, to)

  private def countNonIdentical(from: Board, to: Board): Int = {
    @tailrec
    def recurse(fromGroupsToConsider: List[Group], remainingTo: Board, acc: Int): Int =
      fromGroupsToConsider match {
        case Nil => acc
        case head :: tail =>
          if (remainingTo.contains(head)) recurse(tail, remainingTo - head, acc)
          else recurse(tail, remainingTo, acc + 1)
      }

    recurse(from.groups, to, 0)
  }
}
