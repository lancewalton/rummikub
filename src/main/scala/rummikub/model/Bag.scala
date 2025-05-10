package rummikub.model

import cats.syntax.all.*

import scala.util.Random

case class Bag private (pieces: Map[Piece, Int]) {
  lazy val valueOnRack: Int = pieces.map { case (p, n) => p.valueOnRack * n }.sum

  override def toString: String = {
    implicit val ordering: Ordering[Piece] = (p1: Piece, p2: Piece) => (p1, p2) match {
      case (Piece.Joker, Piece.Joker) => 0
      case (Piece.Joker, _) => -1
      case (_, Piece.Joker) => 1
      case (f1: Piece.Fixed, f2: Piece.Fixed) =>
        if (f1.number < f2.number) -1
        else if (f1.number > f2.number) 1
        else (f1.colour, f2.colour) match {
          case (Colour.Red, Colour.Red) | (Colour.Blue, Colour.Blue) | (Colour.Black, Colour.Black) | (Colour.Yellow, Colour.Yellow) => 0
          case (_, Colour.Red) => 1
          case (Colour.Red, _) => -1
          case (Colour.Blue, _) => -1
          case (_, Colour.Blue) => 1
          case (Colour.Black, _) => -1
          case (_, Colour.Black) => 1
        }
    }

    pieces.toList.sortBy(_._1).map { case (p, n) =>
      if (n == 1) p
      else s"$p x $n"
    }.mkString(", ")
  }

  def hasPiece(piece: Piece): Boolean = pieces.contains(piece)

  def isEmpty: Boolean = pieces.isEmpty

  def hasGroup(group: Group): Boolean = group.pieces.forall(pieces.contains)

  def -(group: Group): Bag = group.pieces.foldLeft(this) { case (acc, piece) => acc - piece }

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

  def takeRandomPiece: Option[(Piece, Bag)] = {
    if (isEmpty) None
    else {
      val pav = piecesAsVector
      val index = Random.nextInt(pav.size)
      val piece = pav(index)
      val bag = Bag(pav.take(index).toList ::: pav.drop(index + 1).toList)
      Some((piece, bag))
    }
  }

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
