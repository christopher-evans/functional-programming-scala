package three

/**
 * Chapter 3.  Exercise 8.
 */
object ListConstructor {
  def test (): String =
    List.print(
      List.foldRight(List(1, 2, 3),
        Nil: List[Int])(Cons(_, _))
    )(
      x => x.toString
    )

}
