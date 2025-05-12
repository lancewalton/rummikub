package rummikub.ai

import rummikub.model.*

import scala.util.Random

object AI {
  def apply(board: Board, player: Player): Option[BoardAndPlayer] = {
    val maybeBestMoveFromRack =
      findBestMoves(Board.empty, player)
        .map { bestMoves =>
          BestMoves(bestMoves.result.map(bestMove => BoardAndPlayer(board ++ bestMove.board, bestMove.player)))
        }

    val maybeBestMove =
      if (player.firstMove)
        maybeBestMoveFromRack
      else {
        val bestMovesFromBoardAndRack = findBestMoves(board, player)
          .map(best => ChangeMinimiser(board, best.result))
          .map(BestMoves(_))

        (maybeBestMoveFromRack, bestMovesFromBoardAndRack) match {
          case (None, None) => None
          case (m @ Some(_), None) => m
          case (None, m @ Some(_)) => m
          case (mm1 @ Some(m1), mm2 @ Some(m2)) =>
            if (m1.valueLeftOnRack < m2.valueLeftOnRack) mm1
            else if (m1.valueLeftOnRack > m2.valueLeftOnRack) mm2
            else Some(BestMoves(m1.result ::: m2.result))
        }
      }

    maybeBestMove.map { best =>
      val list = best.result.toList
      list(Random.nextInt(list.size))
    }
  }

  private def findBestMoves(board: Board, player: Player): Option[BestMoves] = {
    val boardAndPlayer = BoardAndPlayer(board, player.rack)
    BestMovesFinder(ValidGroupFinder(boardAndPlayer), boardAndPlayer)
      .filterNot(player.firstMove && player.valueOnRack - _.result.head.player.valueOnRack < 30)
  }
}
