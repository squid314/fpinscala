package us.blmq.option

import us.blmq.support.UnitSpec

class OptionTest extends UnitSpec
{
    "An Option" should "behave thusly" in
        {
            val a: Option[String] = Some("a")
            val b: Option[String] = Some("b")
            val n: Option[String] = None

            a.map(_ + " map") shouldBe Some("a map")

            a.getOrElse("asdf") shouldBe "a"
            n.getOrElse("asdf") shouldBe "asdf"

            val fm: (String) => Option[String] = a => if (a.contains("a")) Some("contains") else None
            a.flatMap(fm) shouldBe Some("contains")
            n.flatMap(fm) shouldBe None

            a.filter(_.contains("a")) shouldBe Some("a")
            a.filter(_.contains("b")) shouldBe None

            a.orElse(b) shouldBe Some("a")
            n.orElse(b) shouldBe Some("b")
            n.orElse(n) shouldBe None
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
            Option.variance(List(1, 2, 2, 2, 2, 2, 2, 3)) shouldBe Some(0.25)
        }
}
