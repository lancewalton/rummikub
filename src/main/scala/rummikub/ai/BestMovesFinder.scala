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
    recurse(possibleGroups.toList.sorted, boardAndPlayer.board.pieces, boardAndPlayer.transferAllToPlayer, None, boardAndPlayer.player.valueOnRack)
  }

  private def recurse(possibleGroups: List[Group], remainingRequiredPieces: Bag, currentBoardAndPlayer: BoardAndPlayer, best: Option[BestMoves], initialValueOnRackToBeat: Int): Option[BestMoves] =
    possibleGroups match {
      case Nil =>
        if (remainingRequiredPieces.isEmpty && initialValueOnRackToBeat > currentBoardAndPlayer.player.valueOnRack)
          (best, currentBoardAndPlayer) match {
              case (None, BoardAndPlayer(Board.empty, _)) => None
              case (None, bap) =>
                println(s"New best found. Value left on rack = ${bap.player.valueOnRack}.")
                Some(BestMoves(NonEmptyList.of(bap)))
              case (Some(currentBest), BoardAndPlayer(Board.empty, _)) => Some(currentBest)
              case (Some(currentBest), bap) =>
                if (currentBest.valueLeftOnRack > bap.player.valueOnRack) {
                  println(s"New best found. Value left on rack = ${bap.player.valueOnRack}.")
                  Some(BestMoves(NonEmptyList.of(bap)))
                } else if (currentBest.valueLeftOnRack === bap.player.valueOnRack)
                  Some(BestMoves(bap :: currentBest.result))
                else Some(currentBest)
            }
        else best

      case head :: tail =>
        val tailResult = recurse(tail, remainingRequiredPieces, currentBoardAndPlayer, best, initialValueOnRackToBeat)

        val remainingRequiredPiecesWithoutHead = remainingRequiredPieces - head
        val boardWithHead = currentBoardAndPlayer.moveGroupFromPlayerToBoard(head)

        recurse(
          possibleGroups.filter(boardWithHead.player.hasGroup),
          remainingRequiredPiecesWithoutHead,
          boardWithHead,
          tailResult,
          initialValueOnRackToBeat
        )
    }
}
