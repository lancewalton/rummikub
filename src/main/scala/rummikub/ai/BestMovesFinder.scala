package rummikub.ai

import cats.data.NonEmptyList
import cats.syntax.all.*
import rummikub.model.*
import scala.annotation.tailrec

object BestMovesFinder {
  def apply(possibleGroups: Set[Group], boardAndPlayer: BoardAndPlayer): Option[BestMoves] = {
    println(s"Finding best moves: Value on rack = ${boardAndPlayer.player.valueOnRack}, possible groups = ${possibleGroups.size}")

    val currentBoardPieces: Bag = boardAndPlayer.board.pieces
    implicit val groupOrdering: Ordering[Group] = Ordering.by[Group, Long](_.pieces.count(currentBoardPieces.hasPiece)).reverse.orElse(Ordering.by[Group, Int](_.size).reverse)

    recurse(possibleGroups.toList.sorted, boardAndPlayer.board.pieces, boardAndPlayer.transferAllToPlayer).filter { 
      _.valueLeftOnRack < boardAndPlayer.player.valueOnRack
    }
  }

  private def recurse(possibleGroups: List[Group], remainingRequiredPieces: Bag, currentBoardAndPlayer: BoardAndPlayer): Option[BestMoves] =
    possibleGroups match {
      case Nil =>
        if (remainingRequiredPieces.nonEmpty)
          None
        else if (currentBoardAndPlayer.board.isEmpty) None
        else Some(BestMoves(NonEmptyList.of(currentBoardAndPlayer)))

      case head :: tail =>
        val tailResult = recurse(tail, remainingRequiredPieces, currentBoardAndPlayer)

        val remainingRequiredPiecesWithoutHead = remainingRequiredPieces - head
        val boardWithHead = currentBoardAndPlayer.moveGroupFromPlayerToBoard(head)

        val headResult = recurse(
          possibleGroups.filter(boardWithHead.player.hasGroup),
          remainingRequiredPiecesWithoutHead,
          boardWithHead
        )

        (headResult, tailResult) match {
          case (None, None) => None
          case (Some(_), None) => headResult
          case (None, Some(_)) => tailResult
          case (Some(bestHead), Some(bestTail)) =>
            if (bestHead.valueLeftOnRack < bestTail.valueLeftOnRack) headResult
            else if (bestHead.valueLeftOnRack < bestTail.valueLeftOnRack) tailResult
            else Some(BestMoves(bestHead.result ::: bestTail.result))
        }
    }
}
