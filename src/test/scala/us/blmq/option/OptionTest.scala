package us.blmq.option

import us.blmq.support.UnitSpec

class OptionTest extends UnitSpec
{
    "An Option" should "behave thusly" in
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

    "The mean method" should "calculate the mean" in
        pending
    "The variance method" should "calculate the variance" in
        pending
}
