package rummikub.model

import cats.syntax.all.*

import scala.util.Random

case class Bag private (pieces: Map[Piece, Int]) {
  lazy val valueOnRack: Int = pieces.map { case (p, n) => p.valueOnRack * n }.sum

  override def toString: String =
    pieces.toList.sortBy(_._1)(Bag.pieceOrdering).map(Bag.renderEntry).mkString(", ")

  def hasPiece(piece: Piece): Boolean = pieces.contains(piece)

  def isEmpty: Boolean = pieces.isEmpty

  def nonEmpty: Boolean = pieces.nonEmpty

  def hasGroup(group: Group): Boolean = group.pieces.forall(pieces.contains)

  def -(group: Group): Bag = group.pieces.foldLeft(this) { case (acc, piece) => acc - piece }

  def --(that: Bag): Bag = that.piecesAsVector.foldLeft(this)(_ - _)

  def contains(that: Bag): Boolean = that.pieces.forall { case (piece, n) => pieces.getOrElse(piece, 0) >= n }

  def -(piece: Piece): Bag =
    pieces
      .get(piece)
      .fold(this) { n =>
        Bag(
          if (n === 1) pieces.removed(piece)
          else pieces + (piece -> (n - 1))
        )
      }

  def +(piece: Piece): Bag = this.add(piece, 1)

  def add(piece: Piece, n: Int): Bag = Bag(pieces + (piece -> (pieces.getOrElse(piece, 0) + n)))

  def ++(bag: Bag): Bag =
    bag.pieces.toList.foldLeft(this) { case (acc, (p, n)) => acc.add(p, n) }

  def numberOfJokers: Int = pieces.getOrElse(Piece.Joker, 0)

  def distinctNonJokers: List[Piece.Fixed] = pieces.keySet.collect {
    case p: Piece.Fixed => p
  }.toList

  def takeRandomPiece: Option[(Piece, Bag)] =
    Option.when(nonEmpty) {
      val index = Random.nextInt(piecesAsVector.size)
      (piecesAsVector(index), removeAt(piecesAsVector, index))
    }

  private def removeAt(pav: Vector[Piece], index: Int): Bag =
    Bag(pav.take(index) ++ pav.drop(index + 1))

  def takeRandomUnsafe(n: Int): (Bag, Bag) =
    if (pieces.size <= n) (this, Bag.empty)
    else {
      val pav = Random.shuffle(piecesAsVector)
      (Bag(pav.take(n).toList), Bag(pav.drop(n)))
    }

  lazy val piecesAsVector: Vector[Piece] = pieces.toVector.foldLeft(Vector.empty[Piece]) { case (acc, (p, n)) =>
    if (n === 1) acc.appended(p)
    else acc.appended(p).appended(p)
  }
}

object Bag {
  val empty: Bag = Bag(Nil)

  private def colourRank(colour: Colour): Int = colour match {
    case Colour.Red    => 0
    case Colour.Blue   => 1
    case Colour.Black  => 2
    case Colour.Yellow => 3
  }

  private val pieceOrdering: Ordering[Piece] = Ordering.by[Piece, (Int, Int, Int)] {
    case Piece.Joker                 => (0, 0, 0)
    case Piece.Fixed(colour, number) => (1, number, colourRank(colour))
  }

  private def renderEntry(entry: (Piece, Int)): String = entry match {
    case (piece, 1)     => piece.toString
    case (piece, count) => s"$piece x $count"
  }

  val initial: Bag = {
    val pieces =
      Piece.Joker :: Piece.Joker :: {
      for {
          colour <- Colour.values.toList
          number <- 1 to 13
          _ <- 0 to 1
        } yield Piece.Fixed(colour, number)
      }


    Bag(Random.shuffle(pieces))
  }

  def apply(pieces: Piece*): Bag = Bag(pieces.toList)

  def apply(pieces: List[Piece]): Bag = new Bag(pieces.groupBy(identity).view.mapValues(_.size).toMap)
  def apply(pieces: Vector[Piece]): Bag = new Bag(pieces.groupBy(identity).view.mapValues(_.size).toMap)
}
