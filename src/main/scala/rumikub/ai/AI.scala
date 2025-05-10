package rumikub.ai

import cats.syntax.all.*
import rumikub.model.*

import scala.annotation.tailrec
import scala.util.Random

object AI {
  def apply(board: Board, player: Player): Option[BoardAndPlayer] = {
    val boardAndPlayer = BoardAndPlayer(board, player.rack)

    BestMovesFinder(ValidGroupFinder(boardAndPlayer), boardAndPlayer)
      .filterNot(player.firstMove && player.valueOnRack - _.result.head.player.valueOnRack < 30)
      .map(best => ChangeMinimiser(board, best.result)).map { nel =>
        val list = nel.toList
        list(Random.nextInt(list.size))
      }
  }
}
