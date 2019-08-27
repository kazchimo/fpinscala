package example

object Answers {
  def fib(n: Int): Int = n match {
    case num if num > 1 => fib(num-1) + fib(num-2)
    case num if num == 1 => 1
    case num if num == 0 => 0
  }
}
