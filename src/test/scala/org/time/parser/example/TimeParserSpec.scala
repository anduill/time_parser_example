package org.time.parser.example

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import org.scalatest.{FlatSpec, Matchers}
import org.time.parser.example.TimeParser.{AM, ActualTime, BaseTime, Hour, Minute, PM, TimeMode}

class TimeParserSpec extends FlatSpec with Matchers {
  "simple service call 1" should "succeed" in {
    val numberRegexString = "[1-9]"
    val numberRegex = numberRegexString.r
    val foundNumber = numberRegex.findFirstIn("11").toList
    println(s"hello there")

    val actualTime = TimeParser.addMinutes("9:13 AM", 200)
    println(s"Actual Time: $actualTime")
  }
}
