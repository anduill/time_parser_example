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
  "time parsing and adding 100 min" should "succeed" in {
    val actualTime = TimeParser.addMinutes("6:39 PM", 100)
    val expectedTime = "8:19 PM"
    actualTime shouldBe expectedTime
  }
  "time parsing and adding 340 min" should "succeed" in {
    val actualTime = TimeParser.addMinutes("6:39 PM", 340)
    val expectedTime = "12:19 AM"
    actualTime shouldBe expectedTime
  }
  "time parsing and adding 189 min" should "succeed" in {
    val actualTime = TimeParser.addMinutes("6:39 PM", 189)
    val expectedTime = "9:48 PM"
    actualTime shouldBe expectedTime
  }
  "time parsing 12:61 AM" should "fail" in {
    assertThrows[IllegalArgumentException](TimeParser.addMinutes("12:61 AM", 0))
  }
  "adding 23 hours" should "be one minute behind" in {
    val actualTime = TimeParser.addMinutes("12:51 AM", 1439)
    val expectedTime = "12:50 AM"
    actualTime shouldBe expectedTime
  }
  "adding negative one minute should change 12:51 AM to 12:50 AM " should "succeed" in {
    val actualTime = TimeParser.addMinutes("12:51 AM", -1)
    val expectedTime = "12:50 AM"
    actualTime shouldBe expectedTime
  }
  "adding negative two hundred minutes should change 12:51 AM to 9:31 AM " should "succeed" in {
    val actualTime = TimeParser.addMinutes("12:51 AM", -200)
    val expectedTime = "9:31 AM"
    actualTime shouldBe expectedTime
  }
  "time parsing 13:51 AM" should "fail" in {
    assertThrows[IllegalArgumentException](TimeParser.addMinutes("13:51 AM", 0))
  }
}
