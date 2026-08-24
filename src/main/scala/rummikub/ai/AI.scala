package rummikub.ai

import rummikub.ai.cpsat.MoveSolver
import rummikub.model.*

object AI:
  def apply(board: Board, player: Player): Option[BoardAndPlayer] =
    MoveSolver(board, player.rack, player.firstMove)
