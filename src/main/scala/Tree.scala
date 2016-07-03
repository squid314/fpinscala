
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree
{
    def size[A](t: Tree[A]): Int = t match
    {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
    }

    def max(t: Tree[Int]): Int = t match
    {
    case Leaf(i) => i
    case Branch(l, r) => max(l) max max(r)
    }

    def depth[A](t: Tree[A]): Int = t match
    {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match
    {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def fsize[A](t: Tree[A]) =
        fold(t)(_ => 1)(1 + _ + _)

    def fmax(t: Tree[Int]) =
        fold(t)(i => i)(_ max _)

    def fdepth[A](t: Tree[A]) =
        fold(t)(_ => 0)((i, j) => (i max j) + 1)

    def fmap[A, B](t: Tree[A])(f: A => B) =
        fold(t)(a => Leaf(f(a)).asInstanceOf[Tree[A]])((l, r) => Branch(l, r))
}

object TreeCode
{
    def callme()
    {
        import Tree._

        val line = Branch(
            Leaf(0),
            Branch(
                Leaf(1),
                Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7))))))
            )
        )
        val bal = Branch(
            Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3))),
            Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(6), Leaf(7)))
        )
        val stuff = Branch(Branch(Leaf(2), Leaf(9)), Branch(Leaf(4), Leaf(7)))

        println
        println("size: " + size(Leaf(0)))
        println("size: " + size(Branch(Leaf(0), Leaf(0))))
        println("size: " + size(line))
        println("size: " + size(bal))
        println("max: " + max(Branch(Leaf(0), Branch(Leaf(4), Leaf(0)))))
        println("max: " + max(Branch(Leaf(0), Branch(Leaf(4), Leaf(7)))))
        println("max: " + max(stuff))
        println("depth: " + depth(line))
        println("depth: " + depth(bal))
        println("map: " + map(line)(a => if (a % 3 == 0) a / 3 else a * 2 + 1))
        println("map: " + map(bal)(a => if (a % 3 == 0) a / 3 else a * 2 + 1))
        println("map: " + map(bal)(a => if (a % 2 == 0) "even" else "odd"))
        println
        println("fsize: " + fsize(line))
        println("fmax: " + fmax(stuff))
        println("fdepth: " + fdepth(line))
        println("fdepth: " + fdepth(bal))
        println("fmap: " + fmap(bal)(a => if (a % 3 == 0) a / 3 else a * 2 + 1))
        println("fmap: " + fmap(bal)(a => if (a % 2 == 0) "even" else "odd"))
    }
}
