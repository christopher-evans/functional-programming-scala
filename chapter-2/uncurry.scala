
object exercise {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit =
    println(
      "%d".format(
        uncurry((a: Int) => (b: Int) => a - b).apply(1, -1)
      )
    )
    println(
      "%d".format(
        uncurry((a: Int) => (b: Int) => a - b).apply(1, 2)
      )
    )
    println(
      "%d".format(
        uncurry((a: Int) => (b: Int) => a - b).apply(1, -2)
      )
    )
}