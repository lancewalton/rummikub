package rummikub.frontend

import com.raquo.laminar.api.L.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import rummikub.model.{Colour, RoomCode}
import rummikub.play.*
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

object Main:
  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(dom.document.getElementById("app"), App().node)

private final case class TileFace(label: String, background: String, text: String)

final class App:
  private val incoming  = new EventBus[ServerMessage]
  private val name      = Var("")
  private val code      = Var("")
  private val connected = Var(true)
  private val workspace = Var(Workspace(Nil, 0))
  private val dragging  = Var(Option.empty[Int])
  private var socket: Option[dom.WebSocket] = None

  private val state: Signal[AppState] = incoming.events.scanLeft(AppState.initial)(AppState.reduce)
  private val phase: Signal[Phase]    = state.map(_.phase)
  private val game: Signal[Option[GameStateView]] = state.map(_.game)
  private val yourTurn: Signal[Boolean] = state.map(_.yourTurn)
  // Actions require your turn AND a live connection, so nothing silently no-ops.
  private val active: Signal[Boolean] = yourTurn.combineWith(connected.signal).map((turn, ok) => turn && ok)
  private val commitDisabled: Signal[Boolean] =
    active.combineWith(workspace.signal.map(_.canCommit)).map((canAct, committable) => !(canAct && committable))
  // You may only draw before touching the board — otherwise commit or reset first.
  private val boardPristine: Signal[Boolean] =
    workspace.signal.combineWith(game).map((ws, view) => view.forall(v => ws.boardGroups == v.board.groups.map(_.tiles)))
  private val drawDisabled: Signal[Boolean] =
    active.combineWith(boardPristine).map((canAct, pristine) => !(canAct && pristine))

  def node: HtmlElement =
    div(
      onMountCallback(_ => connect()),
      game.changes.collect { case Some(view) => view }.withCurrentValueOf(workspace.signal)
        .map((view, ws) => ws.syncTo(view.board.groups.map(_.tiles), view.yourTiles)) --> workspace,
      child.maybe <-- connected.signal.map(ok => Option.unless(ok)(p(color := "crimson", fontWeight := "bold", "Connection lost — reload the page to reconnect."))),
      joiningSection,
      lobbySection,
      gameSection
    )

  private def joiningSection: HtmlElement =
    section(Phase.Joining)(
      h1("Rummikub"),
      p("Enter your name, then create a game or join one with its code."),
      div(input(placeholder := "Your name", controlled(value <-- name, onInput.mapToValue --> name))),
      div(
        marginTop := "0.5rem",
        button(
          tpe := "button",
          "Create game",
          onClick.compose(_.withCurrentValueOf(name.signal).map((_, n) => n).filter(_.nonEmpty))
            --> Observer[String](n => send(ClientMessage.CreateRoom(n)))
        )
      ),
      div(
        marginTop := "0.5rem",
        input(placeholder := "Game code", controlled(value <-- code, onInput.mapToValue.map(_.toUpperCase) --> code)),
        button(
          tpe := "button",
          "Join game",
          onClick.compose(_.withCurrentValueOf(name.signal.combineWith(code.signal)).filter((_, n, c) => n.nonEmpty && c.nonEmpty))
            --> Observer[(dom.MouseEvent, String, String)] { (_, n, c) => send(ClientMessage.JoinRoom(RoomCode(c), n)) }
        )
      ),
      child.maybe <-- state.map(_.notice.map(reason => p(color := "crimson", reason)))
    )

  private def lobbySection: HtmlElement =
    section(Phase.Lobby)(
      h2("Lobby"),
      p(
        "Game code: ",
        span(fontWeight := "bold", fontSize := "1.1rem", child.text <-- state.map(_.roomCode.map(_.value).getOrElse("…"))),
        " — share it to invite players."
      ),
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
      button(tpe := "button", "Reset board", disabled <-- active.map(!_),
        onClick.compose(_.withCurrentValueOf(game)) --> Observer[(dom.MouseEvent, Option[GameStateView])] {
          case (_, Some(view)) => workspace.update(_.resetBoard(view.board.groups.map(_.tiles)))
          case _               => ()
        }),
      button(tpe := "button", "Commit move", disabled <-- commitDisabled,
        onClick.compose(_.withCurrentValueOf(workspace.signal)) --> Observer[(dom.MouseEvent, Workspace)] {
          case (_, ws) => send(ClientMessage.SubmitMove(ws.boardGroups))
        }),
      button(tpe := "button", "Draw a tile", disabled <-- drawDisabled, onClick --> Observer[Any](_ => send(ClientMessage.Draw))),
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
    val face = tileFace(tile.view)
    span(
      cls := "tile",
      face.label,
      draggable := editable,
      onDragStart.mapTo(Some(tile.id)) --> dragging,
      display := "inline-block",
      minWidth := "1.5rem",
      fontWeight := "bold",
      textAlign := "center",
      margin := "0.1rem",
      padding := "0.25rem 0.4rem",
      border := "1px solid #00000033",
      borderRadius := "0.25rem",
      cursor := (if editable then "grab" else "default"),
      backgroundColor := face.background,
      color := face.text
    )

  private def tileFace(view: TileView): TileFace = view match
    case TileView.JokerTile             => TileFace("J", "#6a1b9a", "white")
    case TileView.NumberTile(colour, n) => numberTileFace(colour, n)

  private def numberTileFace(colour: Colour, n: Int): TileFace = colour match
    case Colour.Black  => TileFace(n.toString, "#212121", "white")
    case Colour.Blue   => TileFace(n.toString, "#1565c0", "white")
    case Colour.Red    => TileFace(n.toString, "#c62828", "white")
    case Colour.Yellow => TileFace(n.toString, "#f9a825", "black")

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
    ws.onclose = _ => connected.set(false)
    ws.onerror = _ => connected.set(false)
    socket = Some(ws)
