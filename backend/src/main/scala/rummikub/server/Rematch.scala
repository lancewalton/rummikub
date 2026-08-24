package rummikub.server

import rummikub.model.Game

object Rematch:
  def apply(finished: Game): Game =
    Game.initial(finished.playerSequence.map(id => (id, finished.players(id).name)))
