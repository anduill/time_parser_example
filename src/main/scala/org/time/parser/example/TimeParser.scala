package org.time.parser.example

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator._

object TimeParser extends JavaTokenParsers with PackratParsers {
  trait TimeComponent {}
  sealed trait TimeMode extends TimeComponent {
    def regexString: String
    def regex: Regex = regexString.r
    def switch(): TimeMode = {
      if(this == AM){
        PM
      } else {
        AM
      }
    }
  }

  case class Hour(hour: Int) extends TimeComponent
  object Hour{
    val singleHourRegex = "[1-9]".r
    val trailingHourRegex = "[0-2]".r
    val startingHourRegex = "[1-2]".r
  }

  case class Minute(min: Int) extends TimeComponent
  object Minute {
    val startingMinuteRegex = "[0-5]".r
    val trailingMinuteRegex = "[0-9]".r
  }

  object AM extends TimeMode {
    override def regexString: String = "AM"
  }
  object PM extends TimeMode {
    override def regexString: String = "PM"
  }

  case class BaseTime(hour: Hour, minute: Minute)
  case class ActualTime(baseTime: BaseTime, timeMode: TimeMode)

  lazy val baseHourParser: PackratParser[Hour] = (Hour.startingHourRegex ~ Hour.trailingHourRegex | Hour.singleHourRegex) ^^ {
    case leadingNum ~ trailingNum => Hour(s"$leadingNum$trailingNum".toInt)
    case singleNum => Hour(singleNum.toString.toInt)
  }

  lazy val baseMinuteParser: PackratParser[Minute] = (Minute.startingMinuteRegex ~ Minute.trailingMinuteRegex) ^^ {
    case leadingNum ~ trailingNum => Minute(s"$leadingNum$trailingNum".toInt)
  }

  lazy val baseTimeParser: PackratParser[BaseTime] = (baseHourParser ~ ":" ~ baseMinuteParser) ^^ {
    case h ~ _ ~ m => BaseTime(h, m)
  }
  lazy val timeModeParser: PackratParser[TimeMode] = (PM.regex | AM.regex) ^^ {
    case x if x.matches(PM.regexString) => PM
    case x if x.matches(AM.regexString) => AM
  }
  lazy val actualTimeParser: PackratParser[ActualTime] = (baseTimeParser ~ timeModeParser) ^^ {
    case baseTime ~ timeMode => ActualTime(baseTime, timeMode)
  }

  def parseTimeString(timeString: String): Try[ActualTime] = {
    Try {
      val res: TimeParser.ParseResult[ActualTime] = parseAll(actualTimeParser, timeString)
      res match {
        case Error(msg, input) => throw new IllegalArgumentException(s"Parser Error on $input.  With error-message: $msg")
        case Failure(msg, remaining) => throw new IllegalArgumentException(s"Parser Failure, total input ${remaining.source}.  With error-message: $msg")
        case NoSuccess(msg, remaining) => throw new IllegalArgumentException(s"Parser Failure, total input ${remaining.source}.  With error-message: $msg")
        case Success(actualTime, _) => actualTime
      }
    }
  }
  def addMinutes(timeString: String, minutes: Int): Try[ActualTime] = {
    parseTimeString(timeString).map {time => addMinutesHelper(time, minutes)}
  }
  def addMinutesHelper(actualTime: ActualTime, minutes: Int): ActualTime = {
    require(minutes >= 0)
    val baseTime = actualTime.baseTime
    val mins = baseTime.minute
    val hours = baseTime.hour
    val (hoursToAdd, newMins) = processMinutes(mins, minutes)
    val (twelveHourPeriods, hoursLeftOver, newHour) = processHours(hours, hoursToAdd)
    val finalMins = mins.copy(min = newMins)
    val finalHours = hours.copy(hour = newHour)
    val newBaseTime = baseTime.copy(hour = finalHours, minute = finalMins)
    val mode: TimeMode = determineNewMode(actualTime, hours, twelveHourPeriods, hoursLeftOver)
    actualTime.copy(baseTime = newBaseTime, timeMode = mode)
  }
  type TwelveHourPeriods = Int
  type HoursLeftOver = Int
  type NewHour = Int
  type HoursToAdd = Int
  type NewMinutes = Int

  def processHours(hour: Hour, hoursToAdd: HoursToAdd): (TwelveHourPeriods, HoursLeftOver, NewHour) = {
    val twelveHourPeriods = hoursToAdd / 12
    val hoursLeftOver = hoursToAdd % 12
    val newHour = (hour.hour + hoursLeftOver) match {
      case h if h > 12 => h - 12
      case h => h
    }
    (twelveHourPeriods, hoursLeftOver, newHour)
  }

  def processMinutes(minute: Minute, minutesToAdd: Int): (HoursToAdd, NewMinutes) = {
    ((minute.min + minutesToAdd) / 60, (minute.min + minutesToAdd) % 60)
  }

  def determineNewMode(actualTime: ActualTime, hours: Hour, twelveHourPeriods: Int, hoursLeftOver: Int): TimeMode = {
    val switchModeForPeriods = (twelveHourPeriods % 2) == 1
    val crossedOverTwelve = (hours.hour + hoursLeftOver) >= 12
    val switchModes = switchModeForPeriods ^ crossedOverTwelve
    val mode = switchModes match {
      case true => actualTime.timeMode.switch()
      case false => actualTime.timeMode
    }
    mode
  }
}
