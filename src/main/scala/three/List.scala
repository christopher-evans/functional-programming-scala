package three

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * Chapter 3.  Exercise 2.
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, remainder) => remainder
  }

  /**
   * Chapter 3.  Exercise 3.
   */
  def setHead[A](list: List[A], item: A): List[A] = Cons(item, list)

  /**
   * Chapter 3.  Exercise 4.
   */
  @tailrec
  def drop[A](list: List[A], n: Int): List[A] =
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case Cons(_, remainder) => drop(remainder, n - 1)
    }

  /**
   * Chapter 3.  Exercise 5.
   */
  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, remainder) =>
      if (!f(x)) remainder
      else dropWhile(remainder, f)
  }

  /**
   * Chapter 3.  Exercise 6.
   */
  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case default => default
  }

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B =
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * Chapter 3.  Exercise 9.
   */
  def length[A](list: List[A]): Int = foldRight(list, 0)((_, total) => total + 1)

  /**
   * Chapter 3.  Exercise 10.
   */
  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(x, remainder) => foldLeft(remainder, f(z, x))(f)
  }

  /**
   * Chapter 3.  Exercise 11.
   */
  def sum2(list: List[Int]): Int =
    foldLeft(list, 0)((x, y) => x + y)

  /**
   * Chapter 3.  Exercise 11.
   */
  def prod2(list: List[Int]): Int =
    foldLeft(list, 1)((x, y) => x * y)

  /**
   * Chapter 3.  Exercise 12.
   */
  def reverse[A](list: List[A]): List[A] = foldLeft(
    list,
    Nil: List[A]
  )((x, y) => Cons(y, x))

  /**
   * Chapter 3.  Exercise 13.
   */
  def foldRightViaFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(list, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  /**
   * Chapter 3.  Exercise 13.
   */
  def foldLeftViaFoldRight[A, B](list: List[A], z: B)(f: (B, A) => B): B =
    foldRight(list, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  /**
   * Chapter 3.  Exercise 14.
   */
  def append[A](list: List[A], item: A): List[A] = foldRight(
    list,
    Cons(item, Nil)
  )(
    (cursor, result) => Cons(cursor, result)
  )

  /**
   * Chapter 3.  Exercise 15.
   */
  def concat[A](lists: List[List[A]]): List[A] = foldRight(
    lists,
    Nil: List[A]
  )(
    (current, concatenated) => foldRight(
      current,
      concatenated
    )((item, intermediate) => Cons(item, intermediate))
  )

  /**
   * Chapter 3.  Exercise 16.
   */
  def addOne(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(x, remainder) => Cons(x + 1, addOne(remainder))
  }

  /**
   * Chapter 3.  Exercise 17.
   */
  def mapToString(list: List[Int]): List[String] = list match {
    case Nil => Nil
    case Cons(x, remainder) => Cons(x.toString, mapToString(remainder))
  }

  /**
   * Chapter 3.  Exercise 18.
   */
  def map[A, B](list: List[A])(f: A => B): List[B] = list match {
    case Nil => Nil
    case Cons(x, remainder) => Cons(f(x), map(remainder)(f))
  }

  /**
   * Chapter 3.  Exercise 19.
   */
  def filter[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, remainder) =>
      if (f(x)) Cons(x, filter(remainder)(f))
      else filter(remainder)(f)
  }

  /**
   * Chapter 3.  Exercise 19.
   */
  def removeOdd(list: List[Int]): List[Int] = filter(list)(x => x % 2 == 0)

  /**
   * Chapter 3.  Exercise 20.
   */
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = concat(map(list)(f))

  /**
   * Chapter 3.  Exercise 21.
   */
  def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] = flatMap(list)(
    item =>
      if (f(item)) List(item)
      else Nil
  )

  /**
   * Chapter 3.  Exercise 22.
   */
  def sumInPairs(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
    case (x, Nil) => x
    case (Nil, y) => y
    case (Cons(x, leftTail), Cons(y, rightTail)) => Cons(x + y, sumInPairs(leftTail, rightTail))
  }

  /**
   * Chapter 3.  Exercise 23.
   */
  def mapInPairs[A](left: List[A], right: List[A])(f: (A, A) => A): List[A] = (left, right) match {
    case (x, Nil) => x
    case (Nil, y) => y
    case (Cons(x, leftTail), Cons(y, rightTail)) => Cons(f(x, y), mapInPairs(leftTail, rightTail)(f))
  }

  def print[A](list: List[A])(f: A => String): String =
    "[" + foldRight(list, "")((current, next) => f(current) + ", " + next) + "]"
}