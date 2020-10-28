package two

object Fibonacci {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, acc: Int): Int =
      if (n <= 1) {
        prev
      } else if (n <= 2) {
        acc
      } else {
        go(n - 1, acc, prev + acc)
      }

    go(n, 0, 1)
  }

  private def format(x: Int) = {
    val message = "The %d'th fibonacci number is %d"

    message.format(x, fibonacci(x))
  }

  def main(args: Array[String]): Unit =
    println(format(1))
    println(format(2))
    println(format(17))
}
