
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List
{
    def sum(ints: List[Int]): Int = ints match
    {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match
    {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def drop[A](as: List[A], n: Int): List[A] =
        if (n <= 0) as
        else as match
        {
            case Nil => Nil
            case Cons(_, as) => drop(as, n - 1)
        }

    def tail[A](as: List[A]): List[A] = drop(as, 1)

    def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match
    {
        case Nil => Nil
        case Cons(h, t) if p(h) => dropWhile(t)(p)
        case _ => l
    }

    def setHead[A](as: List[A], head: A): List[A] = as match
    {
        case Nil => Nil
        case Cons(_, tail) => Cons(head, tail)
    }

    def init[A](l: List[A]): List[A] = l match
    {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    def append[A](a: List[A], b: List[A]): List[A] =
        frbfl(a, b)((h, t) => Cons(h, t))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match
        {
            case Nil => z
            case Cons(h, t) => f(h, foldRight(t, z)(f))
        }

    def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

    def product2(ds: List[Double]) = foldRight(ds, 1.0)(_ * _)

    def length(l: List[Any]) = foldLeft(l, 0)((length, _) => 1 + length)

    @scala.annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match
        {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    def reverse[A](l: List[A]) =
        foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

    def frbfl[A, B](as: List[A], z: B)(f: (A, B) => B) =
        foldLeft(reverse(as), z)((b, a) => f(a, b))

    def flbfr[A, B](as: List[A], z: B)(f: (B, A) => B) =
        foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def flatten[A](l: List[List[A]]): List[A] =
        foldLeft(reverse(l), Nil: List[A])((m, l) => append(l, m))

    // examples to teach you to see the "map" function
    def plus1(l: List[Int]): List[Int] =
        foldLeft(reverse(l), Nil: List[Int])((l, i) => Cons(i + 1, l))

    def dtos(l: List[Double]): List[String] =
        foldLeft(reverse(l), Nil: List[String])((l, d) => Cons('"' + d.toString + '"', l))

    def map[A, B](l: List[A])(f: A => B): List[B] =
        foldLeft(reverse(l), Nil: List[B])((l, a) => Cons(f(a), l))

    def filter[A](l: List[A])(p: A => Boolean): List[A] =
        foldLeft(reverse(l), Nil: List[A])((l, a) => if (p(a)) Cons(a, l) else l)

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    //flatten(map(l)(f))
        foldLeft(reverse(l), Nil: List[B])((l, a) => append(f(a), l))

    def f2[A](l: List[A])(p: A => Boolean): List[A] =
        flatMap(l)(a => if (p(a)) List(a) else Nil)

    def add(l: List[Int], m: List[Int]): List[Int] =
    {
        @annotation.tailrec
        def loop(l: List[Int], m: List[Int], part: List[Int]): List[Int] =
            l match
            {
                case Nil => part
                case Cons(lh, lt) =>
                    m match
                    {
                        case Nil => part
                        case Cons(mh, mt) =>
                            loop(lt, mt, Cons(lh + mh, part))
                    }
            }
        reverse(loop(l, m, Nil))
    }

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    {
        @annotation.tailrec
        def loop(as: List[A], bs: List[B], part: List[C]): List[C] =
            as match
            {
                case Nil => part
                case Cons(ah, at) =>
                    bs match
                    {
                        case Nil => part
                        case Cons(bh, bt) =>
                            loop(at, bt, Cons(f(ah, bh), part))
                    }
            }
        reverse(loop(as, bs, Nil))
    }

    def hasSubsequence[A](search: List[A], target: List[A]): Boolean =
    {
        @annotation.tailrec
        def check(s: List[A], t: List[A]): Boolean =
            t match
            {
                case Nil => true
                case Cons(th, tt) =>
                    s match
                    {
                        case Nil => false
                        case Cons(sh, st) =>
                            sh == th && check(st, tt)
                    }
            }
        @annotation.tailrec
        def loop(s: List[A]): Boolean =
            s match
            {
                case Nil => false
                case s if check(s, target) => true
                case Cons(sh, st) => loop(st)
            }

        target match
        {
            case Nil => true
            case _ => loop(search)
        }
    }

    def apply[A](as: A*): List[A] =
    {
        @annotation.tailrec
        def stack(part: List[A], l: A*): List[A] =
            if (l.isEmpty)
            {
                part
            }
            else
            {
                stack(Cons(l.head, part), l.tail: _*)
            }
        stack(Nil, as.reverse: _*)
    }
}

object Example
{

    import List._

    println("")
    // book examples
    val ex1: List[Double] = Nil
    println("ex1: " + ex1)
    val ex2: List[Int] = Cons(1, Nil)
    println("ex2: " + ex2)
    val ex3: List[String] = Cons("a", Cons("b", Nil))
    println("ex3: " + ex3)

    // my examples
    val mx1 = List(1, 2, 3, 4, 5)
    println("apply: " + mx1)
    val mx2 = List("a", "b", "c", "d")
    println("apply: " + mx2)
    val mx3 = List(1.1, 2.1, 3.2, 4.3, 5.5, 6.8, 7.13, 8.21, 9.34)
    println("apply: " + mx3)
    println("mx3: " + tail(mx1))
    println("sh: " + setHead(mx2, "head"))
    println("d: " + drop(mx1, 2))
    println("dw1: " + dropWhile(mx1)(_ < 3))
    println("dw2: " + dropWhile(mx1)(_ < 8))
    println("init: " + init(mx1))
    println("append: " + append(List(1, 2, 3, 4), List(-1, -2, -3, -4)))
    println("sum2: " + sum2(mx1))
    println("prod2: " + product2(List(1.2, 3.2, 4.5, 6.77, 5.33)))
    println("len: " + length(mx1))
    println("fr(n,c): " + foldRight(mx1, Nil: List[Int])(Cons.apply))
    println("fl(n,c): " + foldLeft(mx1, Nil: List[Int])((t, h) => Cons(h, t)))
    println("fl sum: " + foldLeft(mx1, 0)(_ + _))
    println("frbfl: " + frbfl(mx1, Nil: List[Int])(Cons.apply))
    println("fr: " + foldRight(mx2, "")(_ + _) + "  frbfl: " + frbfl(mx2, "")(_ + _))
    println("fl: " + foldLeft(mx2, "")(_ + _) + "  frbfl: " + frbfl(mx2, "")(_ + _))
    val s1: Stream[Int] =
    {
        def loop(): Stream[Int] = 1 #:: loop()
        loop()
    }
    val s2: Stream[Stream[Int]] =
    {
        def loop(): Stream[Stream[Int]] = s1.take(500) #:: loop()
        loop()
    }
    val mx4: List[Int] = s2.take(500).map(List(_: _*)).foldLeft(Nil: List[Int])((t, h) => append(h, t))
    println("frbfl: " + frbfl(mx4, 0)(_ + _))
    println("fl: " + foldLeft(mx4, 0)(_ + _))
    println("flbfr: " + flbfr(mx1, 0)(_ + _))
    println("flatten: " + flatten(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
    val mx5 = List(s1.take(500000): _*)
    println("len(mx5): " + length(mx5))
    println("plus1: " + plus1(mx1))
    println("dtos: " + dtos(mx3))
    println("map(_+1): " + map(mx1)(_ + 1))
    println("filter(_%2==0): " + filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0))
    println("flatMap: " + flatMap(mx1)(i => List(i, i + 1, i + 2)))
    println("f2(_%2==1): " + f2(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 1))
    println("add: " + add(List(1, 2, 3, 4, 5), List(3, 4, 1, 5, 2)))
    println("zw(tup): " + zipWith(mx1, mx2)((a, b) => (a, b)))
    println("hs: " + hasSubsequence(mx1, List(1, 3)))
    println("hs: " + hasSubsequence(mx1, List(1, 2)))
    println("hs: " + hasSubsequence(mx1, List(4)))
    println("hs: " + hasSubsequence(mx1, List(5, 4)))
    println("hs: " + hasSubsequence(mx1, List()))
    println("hs: " + hasSubsequence(List(), List()))

    println("")
}
