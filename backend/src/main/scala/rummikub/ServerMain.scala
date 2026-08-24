package rummikub

import cats.effect.*
import com.comcast.ip4s.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import rummikub.server.GameServer

object ServerMain extends IOApp.Simple:
  private val jsPath = Path("frontend/target/scala-3.7.0/rummikub-frontend-fastopt/main.js")

  private def routes(server: GameServer, wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case request @ GET -> Root =>
        StaticFile.fromResource("static/index.html", Some(request)).getOrElseF(NotFound())
      case GET -> Root / "ws" =>
        server.webSocket(wsb)
      case request @ GET -> Root / "main.js" =>
        StaticFile.fromPath(jsPath, Some(request)).getOrElseF(NotFound())
    }

  def run: IO[Unit] =
    GameServer.create.flatMap { server =>
      EmberServerBuilder
        .default[IO]
        .withHost(host"0.0.0.0")
        .withPort(port"8080")
        .withHttpWebSocketApp(wsb => routes(server, wsb).orNotFound)
        .build
        .useForever
    }
