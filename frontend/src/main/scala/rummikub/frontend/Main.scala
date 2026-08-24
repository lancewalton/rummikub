package rummikub.frontend

import com.raquo.laminar.api.L.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import rummikub.model.Colour
import rummikub.play.*
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

object Main:
  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(dom.document.getElementById("app"), App().node)

final class App:
  private val incoming  = new EventBus[ServerMessage]
  private val name      = Var("")
  private val workspace = Var(Workspace(Nil, 0))
  private val dragging  = Var(Option.empty[Int])
  private var socket: Option[dom.WebSocket] = None

  private val state: Signal[AppState] = incoming.events.scanLeft(AppState.initial)(AppState.reduce)
  private val phase: Signal[Phase]    = state.map(_.phase)
  private val game: Signal[Option[GameStateView]] = state.map(_.game)
  private val yourTurn: Signal[Boolean] = state.map(_.yourTurn)
  private val commitDisabled: Signal[Boolean] =
    yourTurn.combineWith(workspace.signal.map(_.canCommit)).map((turn, committable) => !(turn && committable))

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
      h3("Your rack"),
      p(fontSize := "0.85rem", color := "#666", "Arrange your tiles here to spot groups — drag between rows and into the gaps. Move tiles down to the board to build your move."),
      div(
        cls := "rack",
        div(children <-- workspace.signal.map(_.rackRows.map(rowEl(_, editable = true)))),
        newRowZone(Zone.Rack, "＋ new row", editable = true)
      ),
      actions,
      h3("Board"),
      div(
        cls := "board",
        div(children <-- workspace.signal.map(_.boardRows).combineWith(yourTurn).map((rows, editable) => rows.map(rowEl(_, editable)))),
        child <-- yourTurn.map(editable => newRowZone(Zone.Board, "＋ new group", editable))
      )
    )

  private def actions: HtmlElement =
    div(
      marginTop := "1rem",
      button(tpe := "button", "Reset board", disabled <-- yourTurn.map(!_),
        onClick.compose(_.withCurrentValueOf(game)) --> Observer[(dom.MouseEvent, Option[GameStateView])] {
          case (_, Some(view)) => workspace.update(_.resetBoard(view.board.groups.map(_.tiles)))
          case _               => ()
        }),
      button(tpe := "button", "Commit move", disabled <-- commitDisabled,
        onClick.compose(_.withCurrentValueOf(workspace.signal)) --> Observer[(dom.MouseEvent, Workspace)] {
          case (_, ws) => send(ClientMessage.SubmitMove(ws.boardGroups))
        }),
      button(tpe := "button", "Draw a tile", disabled <-- yourTurn.map(!_), onClick --> Observer[Any](_ => send(ClientMessage.Draw))),
      child.maybe <-- state.map(_.notice.map(reason => p(color := "crimson", reason))),
      child.maybe <-- state.map(_.outcome.map(outcomeBanner)),
      child.maybe <-- state.map(_.outcome.map(_ => playAgainButton))
    )

  private def playAgainButton: HtmlElement =
    button(tpe := "button", "Play again", onClick --> Observer[Any](_ => send(ClientMessage.PlayAgain)))

  private def rowEl(row: Row, editable: Boolean): HtmlElement =
    val boardRow = row.zone == Zone.Board
    val valid    = Grouping.isValidGroup(row.tiles.map(_.view))
    div(
      cls := "row",
      display := "flex",
      alignItems := "center",
      margin := "0.25rem 0",
      padding := "0.1rem",
      border := rowBorder(boardRow, valid),
      borderRadius := "0.25rem",
      minHeight := "1.9rem",
      dropTarget(DropTarget.IntoRow(row.id, row.tiles.size), editable),
      slot(row.id, 0, editable) +: row.tiles.zipWithIndex.flatMap((tile, i) => Seq(tileEl(tile, editable), slot(row.id, i + 1, editable))),
      if boardRow && !valid then invalidMarker else emptyNode
    )

  private def rowBorder(boardRow: Boolean, valid: Boolean): String =
    if !boardRow then "1px solid #ccc"
    else if valid then "1px solid #2e7d32"
    else "2px solid crimson"

  private def invalidMarker: HtmlElement =
    span(cls := "invalid", "✗ invalid", color := "crimson", fontSize := "0.8rem", marginLeft := "0.4rem")

  private def slot(rowId: Int, index: Int, editable: Boolean): HtmlElement =
    div(
      cls := "slot",
      dropTarget(DropTarget.IntoRow(rowId, index), editable, stop = true),
      alignSelf := "stretch",
      width := "0.55rem",
      minHeight := "1.5rem",
      borderLeft := "1px dashed #ddd"
    )

  private def newRowZone(zone: Zone, label: String, editable: Boolean): HtmlElement =
    div(
      cls := "new-zone",
      dropTarget(DropTarget.NewRow(zone), editable),
      label,
      display := "flex",
      alignItems := "center",
      justifyContent := "center",
      margin := "0.25rem 0",
      padding := "0.35rem 0.75rem",
      border := "1px dashed #bbb",
      borderRadius := "0.25rem",
      color := "#888"
    )

  private def tileEl(tile: Tile, editable: Boolean): HtmlElement =
    val (label, colour) = tile.view match
      case TileView.JokerTile        => ("J", "grey")
      case TileView.NumberTile(c, n) => (n.toString, cssColour(c))
    span(
      cls := "tile",
      label,
      draggable := editable,
      onDragStart.mapTo(Some(tile.id)) --> dragging,
      display := "inline-block",
      minWidth := "1.5rem",
      textAlign := "center",
      margin := "0.1rem",
      padding := "0.2rem 0.35rem",
      border := "1px solid #333",
      borderRadius := "0.25rem",
      cursor := (if editable then "grab" else "default"),
      color := colour
    )

  private def dropTarget(target: DropTarget, editable: Boolean, stop: Boolean = false): Seq[Modifier[HtmlElement]] =
    if !editable then Seq.empty
    else Seq(
      onDragOver --> Observer[dom.DragEvent](_.preventDefault()),
      onDrop.preventDefault.compose(_.withCurrentValueOf(dragging.signal)) --> Observer[(dom.DragEvent, Option[Int])] {
        case (event, Some(id)) => handleDrop(event, id, target, stop)
        case _                 => ()
      }
    )

  private def handleDrop(event: dom.DragEvent, id: Int, target: DropTarget, stop: Boolean): Unit =
    if stop then event.stopPropagation()
    workspace.update(_.move(id, target))
    dragging.set(None)

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
      if v.currentPlayer == v.you then "Your turn — build a move on the board, then Commit."
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
