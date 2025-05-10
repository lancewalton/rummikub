package rummikub

import java.util.UUID
import rummikub.ai.AI
import rummikub.model.*

import scala.annotation.tailrec

object Main extends App {
  play(Game.initial(List((UUID.randomUUID(), "AI1"), (UUID.randomUUID, "AI2"))))

  @tailrec
  private def play(game: Game): Unit = {
    if (game.isFinished) {
      println()
      println("=" * 120)
      printUnderlined("Finished")
      println
      showBoard(game.board)
      game.players.values.foreach { p =>
        println()
        showPlayer(p)
      }
    }
    else {
      println()
      println("=" * 120)
      println(s"Bag: ${game.bag}")
      showBoard(game.board)
      println()
      showPlayer(game.currentPlayer)

      AI(game.board, game.currentPlayer) match {
        case Some(boardAndPlayer) =>
          println("Playing")
          play(game.update(boardAndPlayer.board, game.currentPlayer.copy(rack = boardAndPlayer.player)))
        case None =>
          println("Taking a piece")
          play(game.noPlayAvailableForCurrentPlayer)
      }
    }
  }

  private def showBoard(board: Board): Unit = {
    printUnderlined("Board")
    board.groups.foreach(showGroup)
  }

  private def showGroup(group: Group): Unit = {
    showPieces(group.pieces.toList)
  }

  private def showPlayer(player: Player): Unit = {
    printUnderlined(s"Player ${player.name}")
    println(player.rack)
  }

  private def showPieces(pieces: Seq[Piece]): Unit = {
    println(
      pieces.map {
        case Piece.Joker => "Joker"
        case Piece.Fixed(c, n) => s"$c $n"
      }.mkString(", ")
    )
  }

  private def printUnderlined(s: String): Unit = {
    println(s)
    println("-" * s.length)
  }
}
