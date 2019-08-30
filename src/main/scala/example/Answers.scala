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
    as.slice(0, as.length-1).zipWithIndex.foldLeft[Boolean](true) {
      case (a, (e, index)) => a && ordered(e, as(index + 1))
    }

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def tail[A](xs: List[A]): List[A] = xs match {
    case _ :: ts => ts
    case Nil => Nil
  }
}
