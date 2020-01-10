package example
import scala.collection.immutable

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
    case Branch(l: Branch[A], _: Leaf[A]) => treeNodeSize(l) + 2
    case Branch(_: Leaf[A], r: Branch[A]) => treeNodeSize(r) + 2
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

  // 3.29
  def treeFold[A, B](tree: Tree[A],
                     lf: Leaf[A] => B,
                     llf: (Leaf[A], Leaf[A]) => B,
                     blf: (Branch[A], Leaf[A]) => B,
                     lbf: (Leaf[A], Branch[A]) => B,
                     bbf: (Branch[A], Branch[A]) => B): B =
    tree match {
      case l: Leaf[A]                         => lf(l)
      case Branch(l: Leaf[A], r: Leaf[A])     => llf(l, r)
      case Branch(l: Branch[A], r: Leaf[A])   => blf(l, r)
      case Branch(l: Leaf[A], r: Branch[A])   => lbf(l, r)
      case Branch(l: Branch[A], r: Branch[A]) => bbf(l, r)
    }

  def foldSize[A](tree: Tree[A]): Int =
    treeFold(
      tree,
      (_: Leaf[A]) => 1,
      (_: Leaf[A], _: Leaf[A]) => 3,
      (l: Branch[A], _: Leaf[A]) => foldSize(l) + 2,
      (_: Leaf[A], r: Branch[A]) => foldSize(r) + 2,
      (l: Branch[A], r: Branch[A]) => foldSize(l) + foldSize(r) + 1
    )

  def foldMaximum(tree: Tree[Int]): Int = treeFold(
    tree,
    (l: Leaf[Int]) => l.value,
    (l: Leaf[Int], r: Leaf[Int]) => l.value max r.value,
    (l: Branch[Int], r: Leaf[Int]) => treeMaximum(l) max r.value,
    (l: Leaf[Int], r: Branch[Int]) => l.value max treeMaximum(r),
    (l: Branch[Int], r: Branch[Int]) => treeMaximum(l) max treeMaximum(r)
  )

  def foldDepth(tree: Tree[_]): Int = treeFold(
    tree,
    (_: Leaf[_]) => 0,
    (_: Leaf[_], _: Leaf[_]) => 1,
    (l: Branch[_], _: Leaf[_]) => 1 + depth(l),
    (_: Leaf[_], r: Branch[_]) => 1 + depth(r),
    (l: Branch[_], r: Branch[_]) => depth(l) max depth(r)
  )

  def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    val mapF = (l: Tree[A], r: Tree[A]) => Branch(treeMap(l, f), treeMap(r, f))
    treeFold(tree, (l: Leaf[A]) => Leaf(f(l.value)), mapF, mapF, mapF, mapF)
  }

  def test3_29() = {
    // for foldSize
    val leaf = Leaf(1)
    val branch = Branch(leaf, leaf)
    val deepBranch = Branch(branch, leaf)

    assert(foldSize(leaf) == 1)
    assert(foldSize(branch) == 3)
    assert(foldSize(deepBranch) == 5)

    // for foldMaximum
    val bigLeaf = Leaf(2)
    val bigBranch = Branch(bigLeaf, deepBranch)

    assert(foldMaximum(branch) == 1)
    assert(foldMaximum(deepBranch) == 1)
    assert(foldMaximum(bigBranch) == 2)

    // for foldDepth
    assert(foldDepth(leaf) == 0)
    assert(foldDepth(branch) == 1)
    assert(foldDepth(deepBranch) == 2)

    // for foldMap
    assert(foldMap(leaf)(_ * 2) == Leaf(2))
    assert(foldMap(branch)(_ + 3) == Branch(Leaf(4), Leaf(4)))
    assert(
      foldMap(deepBranch)(_.toString) == Branch(
        Branch(Leaf("1"), Leaf("1")),
        Leaf("1")
      )
    )
  }

  object EitherAnswers {
    import scala.{Either => _, Option => _}

    // 4.1
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case Some(v) => Some(f(v))
        case None    => None
      }

      def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse None

      def getOrElse[B >: A](default: => B): B = this match {
        case Some(v) => v
        case None    => default
      }

      def orElse[B >: A](ob: => Option[B]): Option[B] = {
        this map (Some(_)) getOrElse ob
      }

      def filter(f: A => Boolean): Option[A] =
        flatMap(a => if (f(a)) Some(a) else None)

      def isDefined: Boolean = this match {
        case Some(_) => true
        case None    => false
      }

      def isEmpty: Boolean = !isDefined
    }
    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]

    def test_41: Unit = {
      val opt = Some(1)
      val none: Option[Int] = None

      assert(opt.map(_ * 2) == Some(2))
      assert(none.map(_ * 2) == None)

      assert(opt.flatMap((a: Int) => Some(a * 2)) == Some(2))
      assert(opt.flatMap((_: Int) => None) == None)
      assert(none.flatMap((_: Int) => Some(1)) == None)

      assert(opt.getOrElse(2) == 1)
      assert(none.getOrElse(2) == 2)

      assert(opt.orElse(Some(2)) == Some(1))
      assert(none.orElse(Some(2)) == Some(2))

      assert(opt.filter((a: Int) => a == 1) == Some(1))
      assert(opt.filter((a: Int) => a == 2) == None)
      assert(none.filter((a: Int) => a == 1) == None)
    }

    // 4.2
    def variance(xs: Seq[Double]): Option[Double] = {
      def mean(xs: Seq[Double]) =
        if (xs.isEmpty) None else Some(xs.sum / xs.length)

      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    // 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        aContent <- a
        bContent <- b
      } yield f(aContent, bContent)

    // 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldLeft(Some(List()): Option[List[A]])(
        (a, e) => map2(a, e)((as, c) => as :+ c)
      )

    // 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil => Some(Nil)
        case head :: tails =>
          for {
            appliedHead <- f(head)
            appliedTail <- traverse(tails)(f)
          } yield appliedHead :: appliedTail
      }

    def test4_5 = {
      def f(i: Int): Option[Int] = if (i <= 10) Some(i) else None
      def g(i: Int): Option[Int] = if (i < 10) Some(i) else None
      val a: immutable.Seq[Int] = for (i <- 0 to 10) yield i

      assert(traverse(a.toList)(f).isDefined)
      assert(traverse(a.toList)(g).isEmpty)
    }

    // 4.6 4.7
    sealed trait Either[+E, +A] {
      def map[B](f: A => B): Either[E, B] = this match {
        case Right(v)   => Right(f(v))
        case v: Left[E] => v
      }

      def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
          case Right(v)   => f(v)
          case v: Left[E] => v
        }

      def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
        this match {
          case v: Right[A] => v
          case _: Left[E]  => b
        }

      def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        (this, b) match {
          case (Right(v1), Right(v2)) => Right(f(v1, v2))
          // (Left, Left)のときどうする？
          case (v: Left[E], _)  => v
          case (_, v: Left[EE]) => v
        }

      def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        es match {
          case Nil => Right(Nil)
          case h :: t =>
            h match {
              case v: Left[E]  => v
              case v: Right[A] => sequence(t).flatMap(l => v.map(_ :: l))
            }
        }

      def traverse[E, A, B](
        as: List[A]
      )(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(Nil)
        case h :: t =>
          f(h) match {
            case v: Left[E]  => v
            case v: Right[B] => traverse(t)(f).flatMap(l => v.map(_ :: l))
          }
      }
    }
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value: A) extends Either[Nothing, A]
  }
}
