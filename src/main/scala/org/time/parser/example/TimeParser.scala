package org.time.parser.example

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator._

object TimeParser extends JavaTokenParsers with PackratParsers {
  trait TimeComponent {}
  sealed trait TimeMode extends TimeComponent {
    def timeModeString: String
    def regex: Regex = timeModeString.r
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
    override def timeModeString: String = "AM"
  }
  object PM extends TimeMode {
    override def timeModeString: String = "PM"
  }

  case class BaseTime(hour: Hour, minute: Minute)
  case class ActualTime(baseTime: BaseTime, timeMode: TimeMode)
  object ActualTime {
    /**
     *
     * @param actualTime the time we want to convert to a string
     * @return string representing this time (e.g. 9:35 AM)
     */
    def serialize(actualTime: ActualTime): String = {
      val hour = actualTime.baseTime.hour.hour.toString
      val mins = actualTime.baseTime.minute.min match {
        case x if x < 10 => s"0$x"
        case x => x.toString
      }
      val mode = actualTime.timeMode.timeModeString
      s"$hour:$mins $mode"
    }
  }

  // Hour parser that needs to differentiate between double digit hours or single digit hours (e.g. 11, 12, 10 vs 9, 6 4)
  lazy val baseHourParser: PackratParser[Hour] = (Hour.startingHourRegex ~ Hour.trailingHourRegex | Hour.singleHourRegex) ^^ {
    case leadingNum ~ trailingNum => Hour(s"$leadingNum$trailingNum".toInt)
    case singleNum => Hour(singleNum.toString.toInt)
  }
  // Minute parser that can either be a double digit minute (e.g. 37, 58) or a single digit (e.g. 09, 02)
  lazy val baseMinuteParser: PackratParser[Minute] = (Minute.startingMinuteRegex ~ Minute.trailingMinuteRegex) ^^ {
    case leadingNum ~ trailingNum => Minute(s"$leadingNum$trailingNum".toInt)
  }
  // Base Time parser for combining the hours and minutes
  lazy val baseTimeParser: PackratParser[BaseTime] = (baseHourParser ~ ":" ~ baseMinuteParser) ^^ {
    case h ~ _ ~ m => BaseTime(h, m)
  }
  // Time Mode parser for determining AM or PM
  lazy val timeModeParser: PackratParser[TimeMode] = (PM.regex | AM.regex) ^^ {
    case x if x.matches(PM.timeModeString) => PM
    case x if x.matches(AM.timeModeString) => AM
  }
  // Actual time is a combination of the time and the TimeMode (i.e. AM or PM)
  lazy val actualTimeParser: PackratParser[ActualTime] = (baseTimeParser ~ timeModeParser) ^^ {
    case baseTime ~ timeMode => ActualTime(baseTime, timeMode)
  }

  /**
   *
   * @param timeString formatted like "[H]H:MM {AM|PM}"
   * @return Try representing a Successful parse or a Failed parse with an explanation
   */
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

  /**
   *
   * @param timeString formatted like "[H]H:MM {AM|PM}"
   * @param minutes is number of minutes to add to the time
   * @return new time with the minutes added
   */
  def addMinutesToActualTime(timeString: String, minutes: Int): Try[ActualTime] = {
    parseTimeString(timeString).map {time => addMinutesHelper(time, minutes)}
  }

  /**
   *
   * @param timeString formatted like "[H]H:MM {AM|PM}"
   * @param minutes is number of minutes to add to the time
   * @return time formatted as "[H]H:MM {AM|PM}" that represents the time with the given number of minutes added
   */
  def addMinutes(timeString: String, minutes: Int): String = {
    addMinutesToActualTime(timeString, minutes) match {
      case scala.util.Success(time) => ActualTime.serialize(time)
      case scala.util.Failure(e) => throw e
    }
  }

  /**
   *
   * @param actualTime is the time we wish to add to
   * @param minutes is number of minutes to add to the time
   * @return new time with the minutes added
   */
  def addMinutesHelper(actualTime: ActualTime, minutes: Int): ActualTime = {
    val baseTime = actualTime.baseTime
    val mins = baseTime.minute
    val hours = baseTime.hour
    val (hoursToAdd, newMins) = processMinutes(mins, minutes)
    val (twelveHourPeriods, hoursLeftOver, newHour) = processHours(hours, hoursToAdd)
    val finalMins = mins.copy(min = newMins)
    val finalHours = hours.copy(hour = newHour)
    val newBaseTime = baseTime.copy(hour = finalHours, minute = finalMins)
    val mode: TimeMode = determineNewMode(actualTime.timeMode, hours, twelveHourPeriods, hoursLeftOver)
    actualTime.copy(baseTime = newBaseTime, timeMode = mode)
  }
  type TwelveHourPeriods = Int
  type HoursLeftOver = Int
  type NewHour = Int
  type HoursToAdd = Int
  type NewMinutes = Int

  /**
   *
   * @param hour current hour
   * @param hoursToAdd hours to add to the current hour
   * @return the number of twelve hour periods, the hours left over (less than 12), and the new hour
   */
  def processHours(hour: Hour, hoursToAdd: HoursToAdd): (TwelveHourPeriods, HoursLeftOver, NewHour) = {
    val twelveHourPeriods = hoursToAdd / 12
    val hoursLeftOver = hoursToAdd % 12
    val newHour = (hour.hour + hoursLeftOver) match {
      case h if h > 12 => h - 12
      case h => h
    }
    (twelveHourPeriods, hoursLeftOver, newHour)
  }

  /**
   *
   * @param minute is current Minute
   * @param minutesToAdd is number of minutes to add to the time
   * @return tuple of hours to add and the leftover minutes
   *         NOTE: to account for subtracting minutes instead of adding minutes,
   *         this routine converts the negative minutes into equivalent positive minutes.
   *         For Example: to calculate processMinutes(9:31 AM, -1), we can call processMinutes(9:31 AM, 1439).
   *         This is the same time as now, but one minute behind.
   */
  def processMinutes(minute: Minute, minutesToAdd: Int): (HoursToAdd, NewMinutes) = {
    if(minutesToAdd < 0){
      val invertedMins = -minutesToAdd
      val negativeRemainderMins = invertedMins % 60
      val negativeRemainderHoursMins = ((invertedMins / 60) % 24) * 60
      val adjustedMins = 1440 - (negativeRemainderHoursMins + negativeRemainderMins)
      processMinutes(minute, adjustedMins)
    } else {
      ((minute.min + minutesToAdd) / 60, (minute.min + minutesToAdd) % 60)
    }
  }

  /**
   *
   * @param timeMode is the current TimeMode
   * @param hours the new hours
   * @param twelveHourPeriods number of twelve hour periods that have passed
   * @param hoursLeftOver number of hours (less than 12) that are left
   * @return new timeMode (i.e. AM or PM) for the new time
   */
  def determineNewMode(timeMode: TimeMode, hours: Hour, twelveHourPeriods: Int, hoursLeftOver: Int): TimeMode = {
    val switchModeForPeriods = twelveHourPeriods % 2
    val crossedOverTwelve = hoursLeftOver > 0 match {
      case true => (hours.hour + hoursLeftOver) >= 12 match {
        case true => 1
        case false => 0
      }
      case false => 0
    }
    val switchModes = ((switchModeForPeriods + crossedOverTwelve) % 2) == 1
    val mode = switchModes match {
      case true => timeMode.switch()
      case false => timeMode
    }
    mode
  }
}
