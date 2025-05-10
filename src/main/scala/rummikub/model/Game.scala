package rummikub.model

import cats.syntax.all.*
import java.util.UUID

case class Game(board: Board, bag: Bag, players: Map[UUID, Player], playerSequence: List[UUID], currentPlayerId: UUID, turnsWithoutChange: Int) {
  def update(updatedBoard: Board, updatedPlayer: Player): Game =
    copy(board = updatedBoard, players = players + (updatedPlayer.id -> updatedPlayer.copy(firstMove = false)), currentPlayerId = nextPlayerId, turnsWithoutChange = 0)

  def noPlayAvailableForCurrentPlayer: Game =
    bag.takeRandomPiece.fold(Game(board, bag, players, playerSequence, nextPlayerId, turnsWithoutChange = turnsWithoutChange + 1)) { case (piece, newBag) =>
      val updatedPlayer = players(currentPlayerId) + piece
      copy(bag = newBag, players = players + (updatedPlayer.id -> updatedPlayer), currentPlayerId = nextPlayerId, turnsWithoutChange = 0)
    }

  private def nextPlayerId: UUID = playerSequence((playerSequence.indexOf(currentPlayerId) + 1) % playerSequence.size)

  def isFinished: Boolean = players.values.exists(_.rack.isEmpty) || turnsWithoutChange === players.size

  def currentPlayer: Player = players(currentPlayerId)
}

object Game {
  def initial(playerIdsAndNames: List[(UUID, String)]): Game = {
    val (bag, players) = createPlayers(playerIdsAndNames)
    Game(Board.empty, bag, players.map(p => p.id -> p).toMap, players.map(_.id), players.head.id, 0)
  }

  private def createPlayers(playerIdsAndNames: List[(UUID, String)]): (Bag, List[Player]) =
    playerIdsAndNames.foldRight((Bag.initial, List.empty[Player])) { case ((id, name), (bag, players)) =>
      val (taken, newBag) = bag.takeRandomUnsafe(14)
      (newBag, Player(id, name, taken, true) :: players)
    }
}