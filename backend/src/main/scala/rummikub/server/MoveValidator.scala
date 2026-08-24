package rummikub.server

import rummikub.model.*

object MoveValidator:
  private val minimumFirstMove = 30

  def apply(game: Game, playerId: PlayerId, proposed: Board): Either[String, Game] =
    val player = game.players(playerId)
    val played = proposed.pieces -- game.board.pieces
    for
      _ <- check(proposed.groups.forall(_.isValid), "Every group must be a valid run or set")
      _ <- check(proposed.pieces.contains(game.board.pieces), "You cannot remove tiles already on the board")
      _ <- check(player.rack.contains(played), "You can only play tiles from your own rack")
      _ <- check(played.nonEmpty, "You must play at least one tile")
      _ <- firstMoveChecks(game.board, player, proposed, played)
    yield game.update(proposed, player.copy(rack = player.rack -- played))

  private def firstMoveChecks(currentBoard: Board, player: Player, proposed: Board, played: Bag): Either[String, Unit] =
    if !player.firstMove then Right(())
    else
      for
        _ <- check(played.valueOnRack >= minimumFirstMove, s"Your first move must be worth at least $minimumFirstMove")
        _ <- check(boardPreserved(currentBoard, proposed), "You cannot rearrange the board on your first move")
      yield ()

  private def boardPreserved(currentBoard: Board, proposed: Board): Boolean =
    currentBoard.groups
      .foldLeft(Option(proposed.groups)) { (remaining, group) =>
        remaining.filter(_.contains(group)).map(removeFirstMatch(group, _))
      }
      .isDefined

  private def check(condition: Boolean, message: String): Either[String, Unit] =
    Either.cond(condition, (), message)
