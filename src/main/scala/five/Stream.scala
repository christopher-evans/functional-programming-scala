package five

import scala.annotation.tailrec
import Stream._

trait Stream[+A] {
  /**
   * Chapter 5.  Exercise 1.
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * Chapter 5.  Exercise 2.
   */
  def take(n: Int): Stream[A] = {
    def go(stream: Stream[A], rem: Int): Stream[A] =
      if (rem <= 0) Empty
      else stream match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => go(t(), rem - 1))
      }

    go(this, n)
  }

  /**
   * Chapter 5.  Exercise 3.
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)(
    (current, accumulator) => p(current) || accumulator
  )

  /**
   * Chapter 5.  Exercise 4.
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)(
    (current, accumulator) => p(current) && accumulator
  )

  /**
   * Chapter 5.  Exercise 5.
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])(
    (current, accumulator) =>
      if (!p(current)) Empty
      else cons(current, accumulator)
  )

  /**
   * Chapter 5.  Exercise 7.
   */
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])(
    (current, accumulator) => cons(f(current), accumulator)
  )

  /**
   * Chapter 5.  Exercise 7.
   */
  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])(
    (current, accumulator) =>
      if (f(current)) cons(current, accumulator)
      else accumulator
  )

  /**
   * Chapter 5.  Exercise 7.
   */
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)(
    (current, accumulator) => cons(current, accumulator)
  )

  /**
   * Chapter 5.  Exercise 8.
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])(
    (current, accumulator) => f(current).append(accumulator)
  )

  def zip[B](right: Stream[B]): Stream[(A, B)] = (this, right) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Cons(aHead, aTail), Cons(bHead, bTail)) => cons((aHead(), bHead()), aTail().zip(bTail()))
  }

  /**
   * Chapter 5.  Exercise 14.
   */
  def startsWith[B >: A](seq: Stream[B]): Boolean = zipAllViaViaUnfold(this, seq)
    .takeWhile { case (_, right) => right.isDefined }
    .foldRight(true)(
      (current, accumulator) => current match {
        case (value, check) =>
          (value == check) && accumulator
      }
    )

  /**
   * Chapter 5.  Exercise 15.
   */
  def tails(): Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case Cons(head, tail) => Some(Cons(head, tail), tail())
  } append Stream(Empty)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  /**
   * Chapter 5.  Exercise 8.
   */
  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = cons(a, s)
    s
  }

  /**
   * Chapter 5.  Exercise 9.
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * Chapter 5.  Exercise 10.
   */
  def fibs(): Stream[Int] = {
    def go(current: Int, previous: Int): Stream[Int] =
      cons(previous, go(current + previous, current))

    go(1, 0)
  }

  /**
   * Chapter 5.  Exercise 11.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  /**
   * Chapter 5.  Exercise 12.
   */
  def onesViaUnfold(): Stream[Int] = unfold(Nil)(Some(1, _))

  /**
   * Chapter 5.  Exercise 12.
   */
  def constantViaUnfold[A](a: A): Stream[A] = unfold(Nil)(Some(a, _))

  /**
   * Chapter 5.  Exercise 12.
   */
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(m => Some((m, m + 1)))

  /**
   * Chapter 5.  Exercise 12.
   */
  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)) {
    case (previous, current) => Some(previous, (current, previous + current))
  }

  /**
   * Chapter 5.  Exercise 13.
   */
  def mapViaUnfold[A, B](stream: Stream[A])(f: A => B): Stream[B] = unfold(stream) {
    case Empty => None
    case Cons(head, tail) => Some(f(head()), tail())
  }

  /**
   * Chapter 5.  Exercise 13.
   */
  def takeViaUnfold[A](stream: Stream[A], n: Int): Stream[A] = unfold((stream, 0)) {
    case (s, m) => s match {
      case Empty => None
      case Cons(head, tail) =>
        if (m >= n) None
        else Some(head(), (tail(), m + 1))
    }
  }

  /**
   * Chapter 5.  Exercise 13.
   */
  def takeWhileViaUnfold[A](stream: Stream[A])(p: A => Boolean): Stream[A] = unfold(stream)(
    f = {
      case Empty => None
      case Cons(head, tail) =>
        if (!p(head())) None
        else Some(head(), tail())
    }
  )

  /**
   * Chapter 5.  Exercise 13.
   */
  def zipViaViaUnfold[A, B](left: Stream[A], right: Stream[B]): Stream[(A, B)] = unfold((left, right)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(lHead, lTail), Cons(rHead, rTail)) =>
      Some((lHead(), rHead()), (lTail(), rTail()))
  }

  /**
   * Chapter 5.  Exercise 13.
   */
  def zipAllViaViaUnfold[A, B](
    left: Stream[A],
    right: Stream[B],
  ): Stream[(Option[A], Option[B])] = unfold((left, right)) {
    case (Empty, Empty) => None
    case (Empty, Cons(head, tail)) => Some((None, Some(head())), (Empty, tail()))
    case (Cons(head, tail), Empty) => Some((Some(head()), None), (tail(), Empty))
    case (Cons(lHead, lTail), Cons(rHead, rTail)) =>
      Some((Some(lHead()), Some(rHead())), (lTail(), rTail()))
  }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (_ startsWith s2)
}