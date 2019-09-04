package example

object Answers {
  def fib(n: Int): Int = {
    def calcFib(n: Int, value1: Int = 1, value2: Int): Int = n match {
      case _ if n > 1 => calcFib(n - 1, value1 + value2, value1)
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
    case _ :: ts => ts
    case Nil | List(_) => Nil
  }

  def setHead[A](xs: List[A], e: A): List[A] = xs match {
    case _ :: ts => e :: ts
    case Nil | List(_) => List(e)
  }

  def drop[A](xs: List[A], n: Int): List[A] = n match {
    case 0 => tail(xs)
    case _ => drop(tail(xs), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: ts => if (f(h)) dropWhile(ts, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case h :: t => h :: init(t)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, ac) => ac + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def fold(xs: List[A], n: Int, prev: B): B = n match {
      case _ if n > 1 => fold(xs.tail, n - 1, f(prev, xs.head))
      case _ if n == 1 => f(prev, xs.head)
    }

    fold(as, as.length, z)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft2(xs, f(z, x))(f)
  }

  def leftSum(ints: List[Int]): Int = foldLeft2(ints, 0)(_ + _)

  def leftProduct(ds: List[Double]): Double = foldLeft2(ds, 1.0)(_ * _)

  def leftLength[A](as: List[A]): Int = foldLeft2(as, 0)((ac, _) => ac + 1)
}
