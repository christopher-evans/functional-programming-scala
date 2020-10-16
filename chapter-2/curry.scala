
object exercise {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def main(args: Array[String]): Unit =
    println(
      "%d".format(
        curry((a: Int, b: Int) => a - b).apply(1).apply(-1)
      )
    )
    println(
      "%d".format(
        curry((a: Int, b: Int) => a - b).apply(1).apply(2)
      )
    )
    println(
      "%d".format(
        curry((a: Int, b: Int) => a - b).apply(1).apply(-2)
      )
    )
}