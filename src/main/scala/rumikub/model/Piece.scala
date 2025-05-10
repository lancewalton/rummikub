package rumikub.model

import cats.Eq

sealed trait Piece {
  def valueOnRack: Int
}

object Piece {
  case object Joker extends Piece {
    override def valueOnRack: Int = 30
  }
  
  case class Fixed(colour: Colour, number: Int) extends Piece {
    override def valueOnRack: Int = number

    override def toString: String = s"$colour $number"
  }

  implicit val equal: Eq[Piece] = Eq.fromUniversalEquals
}