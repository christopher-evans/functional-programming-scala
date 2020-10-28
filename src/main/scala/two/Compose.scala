package two

object Compose {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit =
    println(
      "%d".format(
        compose((b: String) => b.length, (a: Int) => a.toString).apply(1)
      )
    )
    println(
      "%d".format(
        compose((b: String) => b.length, (a: Int) => a.toString).apply(123)
      )
    )
}
