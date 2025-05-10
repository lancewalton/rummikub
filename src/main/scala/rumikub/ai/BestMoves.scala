package rumikub.ai

import cats.data.NonEmptyList

case class BestMoves(result: NonEmptyList[BoardAndPlayer]) {
  def valueLeftOnRack: Int = result.head.player.valueOnRack
}
