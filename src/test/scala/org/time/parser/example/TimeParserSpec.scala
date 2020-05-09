package org.time.parser.example

import org.scalatest.{FlatSpec, Matchers}

class TimeParserSpec extends FlatSpec with Matchers {
  "time parsing and adding 3 hours and 20 min" should "succeed" in {
    val actualTime = TimeParser.addMinutes("9:13 AM", 200)
    val expectedTime = "12:33 PM"
    actualTime shouldBe expectedTime
  }
  "time parsing and adding 0 min" should "succeed" in {
    val actualTime = TimeParser.addMinutes("9:13 AM", 0)
    val expectedTime = "9:13 AM"
    actualTime shouldBe expectedTime
  }
  "time parsing with single min digit" should "succeed" in {
    val actualTime = TimeParser.addMinutes("9:03 AM", 0)
    val expectedTime = "9:03 AM"
    actualTime shouldBe expectedTime
  }
  "time parsing and adding 24 hours" should "succeed" in {
    val actualTime = TimeParser.addMinutes("9:13 AM", 1440)
    val expectedTime = "9:13 AM"
    actualTime shouldBe expectedTime
  }
  "time parsing and adding 12 hours" should "succeed" in {
    val actualTime = TimeParser.addMinutes("9:13 AM", 720)
    val expectedTime = "9:13 PM"
    actualTime shouldBe expectedTime
  }
}
