package four

sealed trait Either[+E, +A] {
  /**
   * Chapter 4.  Exercise 7.
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(error) => Left(error)
  }

  /**
   * Chapter 4.  Exercise 7.
   */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(error) => Left(error)
  }

  /**
   * Chapter 4.  Exercise 7.
   */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(value) => Right(value)
  }

  /**
   * Chapter 4.  Exercise 7.
   */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this flatMap(
    inner => b.map(bInner => f(inner, bInner))
  )
  /**
   * Chapter 4.  Exercise 7.
   */
  def mapBoth[B, EE](f: A => B, g: E => EE): Either[EE, B] = this match {
    case Left(error) => Left(g(error))
    case Right(value) => Right(f(value))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  /**
   * Chapter 4.  Exercise 8.
   */
  def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = traverse(list)(x => x)

  /**
   * Chapter 4.  Exercise 8.
   */
  def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = list match {
    case Nil => Right(Nil)
    case x :: y => f(x).flatMap(b => traverse(y)(f) map (rem => b :: rem))
  }

  /**
   * Chapter 4.  Exercise 9.
   */
  //def traverseMulti[E, A, B](list: List[A])(f: A => Either[E, B]): Either[List[E], List[B]] = Right(Nil)
}
