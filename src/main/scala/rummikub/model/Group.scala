package rummikub.model

import cats.data.NonEmptyList
import java.util.UUID

sealed trait Group {
  def pieces: NonEmptyList[Piece]
  def size: Int = pieces.size
  def isValid: Boolean
}

object Group {
  case class Run(piecesInSequence: NonEmptyList[Piece]) extends Group {
    def pieces: NonEmptyList[Piece] = piecesInSequence
    def isValid: Boolean = size >= 3 && sameColour && consecutiveWithinBounds

    private def fixedWithPosition: List[(Piece.Fixed, Int)] =
      piecesInSequence.toList.zipWithIndex.collect { case (f: Piece.Fixed, i) => (f, i) }

    private def sameColour: Boolean =
      fixedWithPosition.map(_._1.colour).distinct.sizeIs == 1

    private def consecutiveWithinBounds: Boolean =
      fixedWithPosition.map { case (f, i) => f.number - i }.distinct match
        case base :: Nil => base >= 1 && base + size - 1 <= 13
        case _           => false
  }

  case class Number(pieces: NonEmptyList[Piece]) extends Group {
    def isValid: Boolean = (size == 3 || size == 4) && sameNumber && distinctColours

    private def fixed: List[Piece.Fixed] = pieces.toList.collect { case f: Piece.Fixed => f }

    private def sameNumber: Boolean = fixed.map(_.number).distinct.sizeIs == 1

    private def distinctColours: Boolean = fixed.map(_.colour).distinct.sizeIs == fixed.size
  }
}