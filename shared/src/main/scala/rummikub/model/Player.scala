package rummikub.model

case class Player(id: PlayerId, name: String, rack: Bag, firstMove: Boolean) {
  def +(piece: Piece): Player = copy(rack = rack + piece)
  def ++(newPieces: Bag): Player = copy(rack = rack ++ newPieces)
  def -(group: Group): Player = copy(rack = rack - group)
  def hasGroup(group: Group): Boolean = rack.hasGroup(group)
  def valueOnRack: Int = rack.valueOnRack
}