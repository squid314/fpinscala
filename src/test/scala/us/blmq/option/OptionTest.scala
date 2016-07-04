package us.blmq.option

import us.blmq.support.UnitSpec

class OptionTest extends UnitSpec
{
    behavior of "An Option"
    ignore should "behave thusly" in
        {
            val a: Option[String] = Some("a")
            val b: Option[String] = Some("b")
            val n: Option[String] = None

            info("map: " + a.map(_ + " map"))
            info("getOrElse: " + a.getOrElse("asdf"))
            info("getOrElse: " + n.getOrElse("asdf"))
            info("flatMap: " + a.flatMap(a => if (a.contains("a")) Some("contains") else None))
            info("flatMap: " + n.flatMap(a => if (a.contains("a")) Some("contains") else None))
            info("filter: " + a.filter(_.contains("a")))
            info("filter: " + a.filter(_.contains("b")))
            info("orElse: " + a.orElse(b))
            info("orElse: " + n.orElse(b))
            info("orElse: " + n.orElse(n))
        }

    "The mean method" should "return None for an empty list" in
        {
            Option.mean(List()) shouldBe None
        }
    it should "return the number for a single number" in
        {
            Option.mean(List(1)) shouldBe Some(1)
        }
    it should "return the same number for a list of repeats" in
        {
            Option.mean(List(1, 1, 1, 1)) shouldBe Some(1)
        }
    it should "calculate the mean of many numbers" in
        {
            Option.mean(List(1, 2, 3)) shouldBe Some(2)
            Option.mean(List(1, 2, 3, 4, 5)) shouldBe Some(3)
        }

    "The variance method" should "return None for an empty list" in
        {
            Option.variance(List()) shouldBe None
        }
    it should "return 0 for a single number" in
        {
            Option.variance(List(1)) shouldBe Some(0)
        }
    it should "return 0 for a list of repeats" in
        {
            Option.variance(List(1, 1)) shouldBe Some(0)
            Option.variance(List(1, 1, 1)) shouldBe Some(0)
            Option.variance(List(3, 3, 3, 3)) shouldBe Some(0)
        }
    it should "return the variance for lists" in
        {
            Option.variance(List(1, 3)) shouldBe Some(1)
            Option.variance(List(1, 2, 2, 3)) shouldBe Some(0.5)
            Option.variance(List(1, 2,2,2,2,2, 2, 3)) shouldBe Some(0.25)
        }
}
