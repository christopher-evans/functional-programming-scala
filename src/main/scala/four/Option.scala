package four

sealed trait Option[+A] {
  /**
   * Chapter 4.  Exercise 1.
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  /**
   * Chapter 4.  Exercise 1.
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  /**
   * Chapter 4.  Exercise 1.
   */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  /**
   * Chapter 4.  Exercise 1.
   */
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)

  /**
   * Chapter 4.  Exercise 1.
   */
  def filter(f: A => Boolean): Option[A] = flatMap(x =>
    if (f(x)) Some(x)
    else None
  )
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /**
   * Chapter 4.  Exercise 5.
   */
  def sequence[A](list: List[Option[A]]): Option[List[A]] = list match {
    case Nil => Some(Nil)
    case x :: y => x.flatMap(head => sequence(y) map (rem => head :: rem))
  }

  /**
   * Chapter 4.  Exercise 5.
   */
  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = list match {
    case Nil => Some(Nil)
    case x :: y => f(x).flatMap(b => traverse(y)(f) map (rem => b :: rem))
  }

  /**
   * Chapter 4.  Exercise 5.
   */
  def sequenceViaReverse[A](list: List[Option[A]]): Option[List[A]] = traverse(list)(x => x);
}