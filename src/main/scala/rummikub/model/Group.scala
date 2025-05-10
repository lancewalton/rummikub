package rummikub.model

import cats.data.NonEmptyList
import java.util.UUID

sealed trait Group {
  def pieces: NonEmptyList[Piece]
  def size: Int = pieces.size
}

object Group {
  case class Run(piecesInSequence: NonEmptyList[Piece]) extends Group {
    def pieces: NonEmptyList[Piece] = piecesInSequence
  }

  case class Number(pieces: NonEmptyList[Piece]) extends Group
}