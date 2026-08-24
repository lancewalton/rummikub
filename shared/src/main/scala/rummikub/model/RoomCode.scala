package rummikub.model

opaque type RoomCode = String

object RoomCode:
  def apply(value: String): RoomCode = value

  extension (code: RoomCode) def value: String = code
