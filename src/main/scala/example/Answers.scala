package example

object Answers {
  def fib(n: Int): Int = {
    def calcFib(n: Int, value1: Int = 1, value2: Int): Int = n match {
      case _ if n > 1  => calcFib(n - 1, value1 + value2, value1)
      case _ if n == 1 => value1
      case _ if n == 0 => 0
    }

    calcFib(n, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as.slice(0, as.length - 1).zipWithIndex.foldLeft[Boolean](true) {
      case (a, (e, index)) => a && ordered(e, as(index + 1))
    }

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def tail[A](xs: List[A]): List[A] = xs match {
    case _ :: ts       => ts
    case Nil | List(_) => Nil
  }

  def setHead[A](xs: List[A], e: A): List[A] = xs match {
    case _ :: ts       => e :: ts
    case Nil | List(_) => List(e)
  }

  def drop[A](xs: List[A], n: Int): List[A] = n match {
    case 0 => tail(xs)
    case _ => drop(tail(xs), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil     => Nil
    case h :: ts => if (f(h)) dropWhile(ts, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil      => Nil
    case _ :: Nil => Nil
    case h :: t   => h :: init(t)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, ac) => ac + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def fold(xs: List[A], n: Int, prev: B): B = n match {
      case _ if n > 1  => fold(xs.tail, n - 1, f(prev, xs.head))
      case _ if n == 1 => f(prev, xs.head)
    }

    fold(as, as.length, z)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil     => z
    case x :: xs => foldLeft2(xs, f(z, x))(f)
  }

  def leftSum(ints: List[Int]): Int = foldLeft2(ints, 0)(_ + _)

  def leftProduct(ds: List[Double]): Double = foldLeft2(ds, 1.0)(_ * _)

  def leftLength[A](as: List[A]): Int = foldLeft2(as, 0)((ac, _) => ac + 1)

  def reverse[A](as: List[A]): List[A] =
    as.foldLeft(Nil: List[A])((ac, e) => e :: ac)

  def foldLeftByFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as.reverse.foldRight(z)((a, b) => f(b, a))

  def foldLeftByFoldRight1[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as.foldRight((b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as.foldLeft((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, b) => a +: b)

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a2, a1)((a, b) => a :+ b)

  // 3.15
  def appendLists[A](as: List[A]*): List[A] =
    foldRight(as.toList, Nil: List[A])((a, b) => a ++ b)

  // 3.16
  def plusOne(as: List[Int]): List[Int] = as.map(_ + 1)

  // 3.17
  def DoubleToString(as: List[Double]): List[String] = as.map(_.toString)

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil    => Nil
    case h :: t => f(h) +: map(t)(f)
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil            => Nil
    case h :: t if f(h) => h +: filter(t)(f)
    case _ :: t         => filter(t)(f)
  }

  def removeOdds(as: List[Int]): List[Int] = filter(as)(_ % 2 == 0)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil    => Nil
    case h :: t => f(h) ++ flatMap(t)(f)
  }

  // 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def zipMerge(as: List[Int], bs: List[Int]): List[Int] = {
    def _zipMerge(longs: List[Int], shorts: List[Int]) =
      longs.take(shorts.length).zip(shorts).map(e => e._1 + e._2)

    if (as.length > bs.length) _zipMerge(as, bs) else _zipMerge(bs, as)
  }

  // 3.23
  def zipWith[A, B](as: List[A], bs: List[A], adder: (A, A) => B): List[B] = {
    def _zipMerge(longs: List[A], shorts: List[A]) =
      longs.take(shorts.length).zip(shorts).map(e => adder(e._1, e._2))

    if (as.length > bs.length) _zipMerge(as, bs) else _zipMerge(bs, as)
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if (sup.length >= sub.length) {
      lazy val subLen = sub.length
      lazy val end = sup.length - subLen

      def judge(start: Int = 0): Boolean =
        if (end < start)
          false
        else {
          if (sup.slice(start, start + subLen) == sub)
            true
          else
            judge(start + 1)
        }

      judge()
    } else false

  // 3.25
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def treeNodeSize[A](tree: Tree[A]): Int = tree match {
    case _: Leaf[A]                       => 1
    case Branch(_: Leaf[A], _: Leaf[A])   => 3
    case Branch(l: Branch[A], _: Leaf[A]) => treeNodeSize(l) + 1
    case Branch(_: Leaf[A], r: Branch[A]) => treeNodeSize(r) + 1
    case Branch(l: Branch[A], r: Branch[A]) =>
      treeNodeSize(l) + treeNodeSize(r) + 1
  }

  // 3.26
  def treeMaximum(tree: Tree[Int]): Int = tree match {
    case l: Leaf[Int]                         => l.value
    case Branch(l: Leaf[Int], r: Leaf[Int])   => l.value max r.value
    case Branch(l: Branch[Int], r: Leaf[Int]) => treeMaximum(l) max r.value
    case Branch(l: Leaf[Int], r: Branch[Int]) => l.value max treeMaximum(r)
    case Branch(l: Branch[Int], r: Branch[Int]) =>
      treeMaximum(l) max treeMaximum(r)
  }

  // 3.27
  def depth(tree: Tree[_]): Int = tree match {
    case _: Leaf[_]                         => 0
    case Branch(_: Leaf[_], _: Leaf[_])     => 1
    case Branch(l: Branch[_], _: Leaf[_])   => 1 + depth(l)
    case Branch(_: Leaf[_], r: Branch[_])   => 1 + depth(r)
    case Branch(l: Branch[_], r: Branch[_]) => depth(l) max depth(r)
  }

  // 3.28
  def treeMap[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case l: Leaf[A]   => Leaf(f(l.value))
    case Branch(l, r) => Branch(treeMap(l, f), treeMap(r, f))
  }
}
