package rummikub.frontend

import com.raquo.laminar.api.L.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import rummikub.model.{Colour, PlayerId}
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

object Main:
  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(dom.document.getElementById("app"), App().node)

final class App:
  private val incoming = new EventBus[ServerMessage]
  private val name     = Var("")
  private var socket: Option[dom.WebSocket] = None

  private val state: Signal[AppState] = incoming.events.scanLeft(AppState.initial)(AppState.reduce)
  private val phase: Signal[Phase]    = state.map(_.phase)
  private val game: Signal[Option[GameStateView]] = state.map(_.game)

  def node: HtmlElement =
    div(
      onMountCallback(_ => connect()),
      joiningSection,
      lobbySection,
      gameSection
    )

  private def joiningSection: HtmlElement =
    section(Phase.Joining)(
      h1("Rummikub"),
      p("Enter your name to join the game."),
      input(
        placeholder := "Your name",
        controlled(value <-- name, onInput.mapToValue --> name)
      ),
      button(
        tpe := "button",
        "Join",
        onClick.compose(_.withCurrentValueOf(name.signal).map((_, n) => n).filter(_.nonEmpty))
          --> Observer[String](n => send(ClientMessage.Join(n)))
      )
    )

  private def lobbySection: HtmlElement =
    section(Phase.Lobby)(
      h2("Lobby"),
      ul(
        children <-- state.map(_.lobby).split(_.id.value) { (_, _, playerSignal) =>
          li(child.text <-- playerSignal.map(p => if p.isAi then s"${p.name} (AI)" else p.name))
        }
      ),
      button(tpe := "button", "Add AI player", onClick --> Observer[Any](_ => send(ClientMessage.AddAi("Bot")))),
      button(tpe := "button", "Start game", onClick --> Observer[Any](_ => send(ClientMessage.Start)))
    )

  private def gameSection: HtmlElement =
    section(Phase.InGame)(
      h2("Game"),
      div(
        cls := "players",
        children <-- game.map(renderPlayers)
      ),
      h3("Board"),
      div(cls := "board", children <-- game.map(view => view.toList.flatMap(_.board.groups).map(groupEl))),
      h3("Your tiles"),
      div(cls := "rack", children <-- game.map(view => view.toList.flatMap(_.yourTiles).map(tileEl)))
    )

  private def renderPlayers(view: Option[GameStateView]): List[HtmlElement] =
    view.toList.flatMap(v => v.players.map(playerEl(_, v.currentPlayer, v.you)))

  private def playerEl(player: PlayerView, current: PlayerId, you: PlayerId): HtmlElement =
    val label = s"${player.name}: ${player.tileCount} tiles"
    div(
      cls := "player",
      fontWeight := (if player.id == current then "bold" else "normal"),
      if player.id == you then s"$label (you)" else label
    )

  private def groupEl(group: GroupView): HtmlElement =
    div(cls := "group", display := "inline-block", margin := "0.25rem", padding := "0.25rem", border := "1px solid #999", group.tiles.map(tileEl))

  private def tileEl(tile: TileView): HtmlElement =
    val (label, colour) = tile match
      case TileView.JokerTile          => ("J", "grey")
      case TileView.NumberTile(c, n)   => (n.toString, cssColour(c))
    span(
      label,
      display := "inline-block",
      minWidth := "1.5rem",
      textAlign := "center",
      margin := "0.1rem",
      padding := "0.2rem 0.35rem",
      border := "1px solid #333",
      borderRadius := "0.25rem",
      color := colour
    )

  private def cssColour(colour: Colour): String = colour match
    case Colour.Black  => "black"
    case Colour.Blue   => "blue"
    case Colour.Red    => "red"
    case Colour.Yellow => "goldenrod"

  private def section(shownIn: Phase)(content: Modifier[HtmlElement]*): HtmlElement =
    div(
      display <-- phase.map(p => if p == shownIn then "block" else "none"),
      content
    )

  private def send(message: ClientMessage): Unit =
    socket.foreach(_.send(message.asJson.noSpaces))

  private def connect(): Unit =
    val location = dom.window.location
    val scheme   = if location.protocol == "https:" then "wss" else "ws"
    val ws       = new dom.WebSocket(s"$scheme://${location.host}/ws")
    ws.onmessage = (event: dom.MessageEvent) =>
      decode[ServerMessage](event.data.toString).foreach(incoming.emit)
    socket = Some(ws)
