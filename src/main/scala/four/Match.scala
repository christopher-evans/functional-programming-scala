package four

import java.util.regex.Pattern
import java.util.regex._



object Match {
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  /**
   * Chapter 4.  Exercise 3.
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aValue => b.map(bValue => f(aValue, bValue)))
  }

  /**
   * Chapter 4.  Exercise 4.
   */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(
    mkMatcher(pat1),
    mkMatcher(pat2)
  )(
    (p1, p2) => p1(s) && p2(s)
  )
}
