package rummikub.frontend

import com.raquo.laminar.api.L.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import rummikub.model.Colour
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

object Main:
  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(dom.document.getElementById("app"), App().node)

final class App:
  private val incoming  = new EventBus[ServerMessage]
  private val name      = Var("")
  private val workspace = Var(Workspace(Nil, Nil, 0))
  private val dragging  = Var(Option.empty[Int])
  private var socket: Option[dom.WebSocket] = None

  private val state: Signal[AppState] = incoming.events.scanLeft(AppState.initial)(AppState.reduce)
  private val phase: Signal[Phase]    = state.map(_.phase)
  private val game: Signal[Option[GameStateView]] = state.map(_.game)
  private val yourTurn: Signal[Boolean] = state.map(_.yourTurn)

  def node: HtmlElement =
    div(
      onMountCallback(_ => connect()),
      game.changes.collect { case Some(view) => workspaceOf(view) } --> workspace,
      joiningSection,
      lobbySection,
      gameSection
    )

  private def joiningSection: HtmlElement =
    section(Phase.Joining)(
      h1("Rummikub"),
      p("Enter your name to join the game."),
      input(placeholder := "Your name", controlled(value <-- name, onInput.mapToValue --> name)),
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
      div(cls := "players", children <-- game.map(renderPlayers)),
      p(fontWeight := "bold", child.text <-- game.map(turnStatus)),
      h3("Board"),
      div(
        cls := "board",
        display := "flex",
        flexWrap := "wrap",
        children <-- workspace.signal.map(_.groups).split(_.id) { (groupId, _, groupSignal) =>
          groupZone(groupId, groupSignal.map(_.tiles))
        },
        newGroupZone
      ),
      h3("Your tiles"),
      div(cls := "rack", dropTarget(DropTarget.ToRack), minHeight := "2rem", padding := "0.25rem", border := "1px dashed #bbb",
        children <-- workspace.signal.map(_.rack).split(_.id)((tileId, initial, _) => tileEl(tileId, initial.view))),
      actions
    )

  private def actions: HtmlElement =
    div(
      marginTop := "1rem",
      button(tpe := "button", "Reset", onClick.compose(_.withCurrentValueOf(game)) --> Observer[(dom.MouseEvent, Option[GameStateView])] {
        case (_, Some(view)) => workspace.set(workspaceOf(view))
        case _               => ()
      }),
      button(tpe := "button", "Commit move", disabled <-- yourTurn.map(!_),
        onClick.compose(_.withCurrentValueOf(workspace.signal)) --> Observer[(dom.MouseEvent, Workspace)] {
          case (_, ws) => send(ClientMessage.SubmitMove(ws.toGroups))
        }),
      button(tpe := "button", "Draw a tile", disabled <-- yourTurn.map(!_), onClick --> Observer[Any](_ => send(ClientMessage.Draw))),
      child.maybe <-- state.map(_.notice.map(reason => p(color := "crimson", reason))),
      child.maybe <-- state.map(_.outcome.map(outcomeBanner))
    )

  private def groupZone(groupId: Int, tilesSignal: Signal[List[Tile]]): HtmlElement =
    div(
      dropTarget(DropTarget.ToGroup(groupId)),
      display := "inline-flex",
      margin := "0.25rem",
      padding := "0.25rem",
      border := "1px solid #999",
      borderRadius := "0.25rem",
      minWidth := "2rem",
      minHeight := "1.8rem",
      children <-- tilesSignal.split(_.id)((tileId, initial, _) => tileEl(tileId, initial.view))
    )

  private def newGroupZone: HtmlElement =
    div(
      dropTarget(DropTarget.NewGroup),
      "＋ new group",
      display := "inline-flex",
      alignItems := "center",
      justifyContent := "center",
      margin := "0.25rem",
      padding := "0.25rem 0.75rem",
      border := "1px dashed #bbb",
      borderRadius := "0.25rem",
      color := "#888"
    )

  private def tileEl(tileId: Int, view: TileView): HtmlElement =
    val (label, colour) = view match
      case TileView.JokerTile        => ("J", "grey")
      case TileView.NumberTile(c, n) => (n.toString, cssColour(c))
    span(
      label,
      draggable := true,
      onDragStart.mapTo(Some(tileId)) --> dragging,
      display := "inline-block",
      minWidth := "1.5rem",
      textAlign := "center",
      margin := "0.1rem",
      padding := "0.2rem 0.35rem",
      border := "1px solid #333",
      borderRadius := "0.25rem",
      cursor := "grab",
      color := colour
    )

  private def dropTarget(target: DropTarget): Seq[Modifier[HtmlElement]] =
    Seq(
      onDragOver --> Observer[dom.DragEvent](_.preventDefault()),
      onDrop.preventDefault.compose(_.withCurrentValueOf(dragging.signal).collect { case (_, Some(id)) => id })
        --> Observer[Int] { id =>
          workspace.update(_.move(id, target))
          dragging.set(None)
        }
    )

  private def renderPlayers(view: Option[GameStateView]): List[HtmlElement] =
    view.toList.flatMap(v => v.players.map(playerEl(_, v.currentPlayer, v.you)))

  private def playerEl(player: PlayerView, current: rummikub.model.PlayerId, you: rummikub.model.PlayerId): HtmlElement =
    val name  = if player.isAi then s"${player.name} (AI)" else player.name
    val label = s"$name: ${player.tileCount} tiles"
    div(
      fontWeight := (if player.id == current then "bold" else "normal"),
      if player.id == you then s"$label (you)" else label
    )

  private def turnStatus(view: Option[GameStateView]): String =
    view.fold("") { v =>
      if v.currentPlayer == v.you then "Your turn — drag tiles onto the board, then Commit."
      else v.players.find(_.id == v.currentPlayer).fold("Waiting…")(p => s"Waiting for ${p.name}…")
    }

  private def outcomeBanner(winner: Option[PlayerView]): HtmlElement =
    p(fontWeight := "bold", winner.fold("Game over — a draw.")(w => s"Game over — ${w.name} wins!"))

  private def workspaceOf(view: GameStateView): Workspace =
    Workspace.fromBoardAndRack(view.board.groups.map(_.tiles), view.yourTiles)

  private def cssColour(colour: Colour): String = colour match
    case Colour.Black  => "black"
    case Colour.Blue   => "blue"
    case Colour.Red    => "red"
    case Colour.Yellow => "goldenrod"

  private def section(shownIn: Phase)(content: Modifier[HtmlElement]*): HtmlElement =
    div(display <-- phase.map(p => if p == shownIn then "block" else "none"), content)

  private def send(message: ClientMessage): Unit =
    socket.foreach(_.send(message.asJson.noSpaces))

  private def connect(): Unit =
    val location = dom.window.location
    val scheme   = if location.protocol == "https:" then "wss" else "ws"
    val ws       = new dom.WebSocket(s"$scheme://${location.host}/ws")
    ws.onmessage = (event: dom.MessageEvent) =>
      decode[ServerMessage](event.data.toString).foreach(incoming.emit)
    socket = Some(ws)
