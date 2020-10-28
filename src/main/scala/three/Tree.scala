package three

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * Chapter 3.  Exercise 25.
   */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  /**
   * Chapter 3.  Exercise 26.
   */
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  /**
   * Chapter 3.  Exercise 27.
   */
  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
  }

  /**
   * Chapter 3.  Exercise 28.
   */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
   * Chapter 3.  Exercise 29.
   */
  def fold[A, B](list: Tree[A])(map: A => B)(f: (B, B) => B): B = list match {
    case Leaf(value) => map(value)
    case Branch(left, right) => f(fold(left)(map)(f), fold(right)(map)(f))
  }

  /**
   * Chapter 3.  Exercise 29.
   */
  def foldSize[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((left, right) => left + right + 1);

  /**
   * Chapter 3.  Exercise 29.
   */
  def foldMax(tree: Tree[Int]): Int = fold(tree)(x => x)((left, right) => left max right);

  /**
   * Chapter 3.  Exercise 29.
   */
  def foldDepth(tree: Tree[Int]): Int = fold(tree)(_ => 0)((left, right) => (left + 1) max (right + 1));

  /**
   * Chapter 3.  Exercise 29.
   */
  def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])((left, right) => Branch(left, right));
}
