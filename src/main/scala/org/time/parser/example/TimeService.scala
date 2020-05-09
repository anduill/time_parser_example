package org.time.parser.example

import cats.effect.IO
import io.circe.Json
import io.circe.syntax._
import org.http4s.HttpService
import org.http4s.circe._
import org.http4s.dsl.impl.Root
import org.http4s.dsl.io._

import scala.util.{Failure, Success}

object TimeStringParam extends QueryParamDecoderMatcher[String]("time")
object MinutesIntParam extends QueryParamDecoderMatcher[Int]("minutes")
object TimeService {
  def getTimeService(): HttpService[IO] = {
    HttpService[IO] {
      case req @ GET -> Root / "time" :? TimeStringParam(timeString) :? MinutesIntParam(mins) =>
        TimeParser.addMinutes(timeString, mins) match {
          case Failure(exception) =>
            BadRequest(Json.obj("message" -> exception.getMessage.asJson))
          case Success(value) =>
            Ok(Json.obj("value" -> value.toString.asJson))
        }
    }
  }
}
