package rummikub.ai

import cats.data.NonEmptyList
import cats.syntax.all.*
import rummikub.model.*

import scala.annotation.tailrec

object ChangeMinimiser {
  def apply(original: Board, choices: NonEmptyList[BoardAndPlayer]): NonEmptyList[BoardAndPlayer] = {
    println("Minimising changes")
    recurse(original, choices.tail, NonEmptyList.of(choices.head), BoardDiffScorer(original, choices.head.board))
  }

  @tailrec
  private def recurse(original: Board, choices: List[BoardAndPlayer], result: NonEmptyList[BoardAndPlayer], bestDiffs: Int): NonEmptyList[BoardAndPlayer] =
    choices match {
      case Nil => result
      case head :: tail =>
        val headDiffs = BoardDiffScorer(original, head.board)
        if (headDiffs > bestDiffs) recurse(original, tail, result, bestDiffs)
        else if (headDiffs === bestDiffs) recurse(original, tail, head :: result, bestDiffs)
        else recurse(original, tail, NonEmptyList.of(head), headDiffs)
    }
}
