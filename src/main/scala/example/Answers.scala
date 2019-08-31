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
}
