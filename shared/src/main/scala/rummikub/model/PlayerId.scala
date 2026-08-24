package rummikub.model

opaque type PlayerId = String

object PlayerId:
  def apply(value: String): PlayerId = value

  extension (id: PlayerId) def value: String = id
