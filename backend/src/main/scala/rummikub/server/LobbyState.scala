package rummikub.server

import rummikub.model.{Game, PlayerId}
import rummikub.protocol.LobbyPlayer

final case class LobbyMember(id: PlayerId, name: String, isAi: Boolean)

final case class LobbyState(members: Vector[LobbyMember]):
  def join(id: PlayerId, name: String): LobbyState  = add(LobbyMember(id, name, isAi = false))
  def addAi(id: PlayerId, name: String): LobbyState = add(LobbyMember(id, name, isAi = true))

  def remove(id: PlayerId): LobbyState = LobbyState(members.filterNot(_.id == id))

  def toLobbyPlayers: List[LobbyPlayer] = members.map(m => LobbyPlayer(m.id, m.name, m.isAi)).toList

  def startGame: Option[Game] =
    Option.when(members.sizeIs >= 2)(Game.initial(members.map(m => (m.id, m.name)).toList))

  private def add(member: LobbyMember): LobbyState = LobbyState(members :+ member)

object LobbyState:
  val empty: LobbyState = LobbyState(Vector.empty)
