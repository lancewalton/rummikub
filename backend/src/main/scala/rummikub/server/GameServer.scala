package rummikub.server

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.Response
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import rummikub.model.{PlayerId, RoomCode}
import rummikub.protocol.*
import rummikub.protocol.Codecs.given

import java.util.UUID
import scala.concurrent.duration.*
import scala.util.Random

// Routes each connection to its game room; rooms are created on demand and
// removed once empty, so concurrent games never share state.
final class GameServer(rooms: Ref[IO, Map[RoomCode, GameRoom]]):
  def webSocket(wsb: WebSocketBuilder2[IO]): IO[Response[IO]] =
    for
      myId   <- newId
      queue  <- Queue.unbounded[IO, ServerMessage]
      myRoom <- Ref.of[IO, Option[GameRoom]](None)
      _      <- queue.offer(ServerMessage.Welcome(myId))
      response <- wsb.build(outbound(queue), inbound(myId, queue, myRoom))
    yield response

  private def outbound(queue: Queue[IO, ServerMessage]): Stream[IO, WebSocketFrame] =
    val messages = Stream.fromQueueUnterminated(queue).map(message => WebSocketFrame.Text(message.asJson.noSpaces))
    // Periodic pings keep an otherwise-idle connection alive (browsers reply with
    // pong automatically), so waiting for your turn never drops the socket.
    val heartbeat = Stream.awakeEvery[IO](20.seconds).map(_ => WebSocketFrame.Ping())
    messages.merge(heartbeat)

  private def inbound(myId: PlayerId, queue: Queue[IO, ServerMessage], myRoom: Ref[IO, Option[GameRoom]]): fs2.Pipe[IO, WebSocketFrame, Unit] =
    _.collect { case WebSocketFrame.Text(text, _) => text }
      .evalMap(text => decode[ClientMessage](text).fold(_ => IO.unit, handle(myId, queue, myRoom, _)))
      .onFinalize(disconnect(myId, myRoom))

  private def handle(myId: PlayerId, queue: Queue[IO, ServerMessage], myRoom: Ref[IO, Option[GameRoom]], message: ClientMessage): IO[Unit] =
    message match
      case ClientMessage.CreateRoom(name)      => createRoom(myId, queue, myRoom, name)
      case ClientMessage.JoinRoom(code, name)  => joinRoom(myId, queue, myRoom, code, name)
      case other                               => myRoom.get.flatMap(_.fold(IO.unit)(_.handle(myId, other)))

  private def createRoom(myId: PlayerId, queue: Queue[IO, ServerMessage], myRoom: Ref[IO, Option[GameRoom]], name: String): IO[Unit] =
    for
      code <- freshCode
      room <- GameRoom.create
      _    <- rooms.update(_ + (code -> room))
      _    <- myRoom.set(room.some)
      _    <- queue.offer(ServerMessage.RoomJoined(code))
      _    <- room.join(myId, queue, name)
    yield ()

  private def joinRoom(myId: PlayerId, queue: Queue[IO, ServerMessage], myRoom: Ref[IO, Option[GameRoom]], code: RoomCode, name: String): IO[Unit] =
    rooms.get.map(_.get(code)).flatMap {
      case Some(room) => myRoom.set(room.some) *> queue.offer(ServerMessage.RoomJoined(code)) *> room.join(myId, queue, name)
      case None       => queue.offer(ServerMessage.RoomNotFound).void
    }

  private def disconnect(myId: PlayerId, myRoom: Ref[IO, Option[GameRoom]]): IO[Unit] =
    myRoom.get.flatMap(_.fold(IO.unit)(room => room.disconnect(myId) *> removeIfEmpty(room)))

  private def removeIfEmpty(room: GameRoom): IO[Unit] =
    room.isEmpty.flatMap(empty => if empty then rooms.update(_.filter((_, r) => r != room)) else IO.unit)

  private def freshCode: IO[RoomCode] =
    IO(RoomCode(List.fill(4)(('A' + Random.nextInt(26)).toChar).mkString)).flatMap { code =>
      rooms.get.map(_.contains(code)).flatMap(taken => if taken then freshCode else IO.pure(code))
    }

  private def newId: IO[PlayerId] = IO(PlayerId(UUID.randomUUID().toString))

object GameServer:
  def create: IO[GameServer] =
    Ref.of[IO, Map[RoomCode, GameRoom]](Map.empty).map(GameServer(_))
