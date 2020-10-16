import scala.runtime.ScalaRunTime._

object exercise {

  def isSorted[T](arr: Array[T], greaterThan: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= arr.length - 1) {
        true
      } else if (greaterThan(arr(n), arr(n + 1))) {
        false
      } else {
        loop(n + 1)
      }
    
    loop(0)
  }

  def print(arr: Array[Int]) = {
    val sorted = isSorted(arr, (left: Int, right: Int) => left > right)
    val str = stringOf(arr)
    val status = if (sorted) "sorted" else "not sorted"

    println(s"The array ${str} is ${status}")
  }

  def main(args: Array[String]): Unit =
    print(Array())
    print(Array(1))
    print(Array(1, 3, 7))
    print(Array(1, 7, 3))
    print(Array(1, 7, 7))
}