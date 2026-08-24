package rummikub.ai

import rummikub.model.*

case class BoardAndPlayer(board: Board, player: Bag) {
  def transferAllToPlayer: BoardAndPlayer = BoardAndPlayer(Board.empty, player ++ board.pieces)

  def moveGroupFromPlayerToBoard(group: Group): BoardAndPlayer =
    BoardAndPlayer(
      board + group,
      player - group
    )
}
