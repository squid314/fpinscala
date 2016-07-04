package us
package blmq
package option

sealed trait Option[+A]
{
    def map[B](f: A => B): Option[B] = this match
    {
        case None => None
        case Some(a) => Some(f(a))
    }
    def flatMap[B](f: A => Option[B]): Option[B] =
        map(f) getOrElse None
    def getOrElse[B >: A](default: => B): B = this match
    {
        case None => default
        case Some(a) => a
    }
    def orElse[B >: A](op: => Option[B]): Option[B] =
        map(Some(_)) getOrElse op
    def filter(f: A => Boolean): Option[A] =
        flatMap(a => if (f(a)) Some(a) else None)
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option
{
    def mean(is: Seq[Double]): Option[Double] =
        ((None: Option[Double]) /: is) ((p, d) => p.map(_ + d).orElse(Some(d))).map(_ / is.size)

    def variance(is: Seq[Double]): Option[Double] =
        mean(is).flatMap(m => mean(is.map(x => Math.pow(x - m, 2))))
}
