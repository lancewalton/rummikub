package rummikub.ai

import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.all.*
import rummikub.model.*

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

object BestMovesFinder {
  private implicit val bestMovesMonoid: Monoid[Option[BestMoves]] = new Monoid[Option[BestMoves]] {
    val empty: Option[BestMoves] = None

    def combine(x: Option[BestMoves], y: Option[BestMoves]): Option[BestMoves] = (x, y) match {
      case (None, None) => None
      case (Some(_), None) => x
      case (None, Some(_)) => y
      case (Some(a), Some(b)) =>
        if (a.valueLeftOnRack < b.valueLeftOnRack) x
        else if (a.valueLeftOnRack > b.valueLeftOnRack) y
        else Some(BestMoves(a.result ::: b.result))
    }
  }

  def apply(possibleGroups: Set[Group], boardAndPlayer: BoardAndPlayer): Option[BestMoves] = {
    println(s"Finding best moves: Value on rack = ${boardAndPlayer.player.valueOnRack}, possible groups = ${possibleGroups.size}")

    val startTime = LocalDateTime.now

    val currentBoardPieces: Bag = boardAndPlayer.board.pieces
    implicit val groupOrdering: Ordering[Group] = Ordering.by[Group, Long](_.pieces.count(currentBoardPieces.hasPiece)).reverse.orElse(Ordering.by[Group, Int](_.size).reverse)

    import scala.collection.parallel.CollectionConverters._
    val bestMove = possibleGroups.toList.sorted.tails.toList.par.map(recurse(_, boardAndPlayer.board.pieces, boardAndPlayer.transferAllToPlayer, mustTakeHead = true, startTime).filter {
        _.valueLeftOnRack < boardAndPlayer.player.valueOnRack
      }
    ).seq.foldLeft(Option.empty[BestMoves])(bestMovesMonoid.combine)

    val timeTaken = startTime.until(LocalDateTime.now, ChronoUnit.SECONDS)
    val timeTakenString = s"Time taken: $timeTaken seconds"

    println(bestMove.fold("No move available. ") { best => s"Best move leaves value on rack = ${best.valueLeftOnRack}. " } + timeTakenString)

    bestMove
  }

  private def recurse(possibleGroups: List[Group], remainingRequiredPieces: Bag, currentBoardAndPlayer: BoardAndPlayer, mustTakeHead: Boolean, startTime: LocalDateTime): Option[BestMoves] =
    possibleGroups match {
      case Nil =>
        if (remainingRequiredPieces.nonEmpty)
          None
        else if (currentBoardAndPlayer.board.isEmpty) None
        else Some(BestMoves(NonEmptyList.of(currentBoardAndPlayer)))

      case head :: tail =>
        if (startTime.until(LocalDateTime.now, ChronoUnit.SECONDS) >= 60)
          None
        else {
          val remainingRequiredPiecesWithoutHead = remainingRequiredPieces - head
          val boardWithHead = currentBoardAndPlayer.moveGroupFromPlayerToBoard(head)

          val headResult = recurse(
            possibleGroups.filter(boardWithHead.player.hasGroup),
            remainingRequiredPiecesWithoutHead,
            boardWithHead,
            mustTakeHead = false,
            startTime
          )

          if (mustTakeHead) headResult
          else {
            val tailResult = recurse(tail, remainingRequiredPieces, currentBoardAndPlayer, mustTakeHead = false, startTime)
            bestMovesMonoid.combine(headResult, tailResult)
          }
        }
    }
}
