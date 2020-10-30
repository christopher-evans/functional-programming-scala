package four

object Variance {
  /**
   * Chapter 4.  Exercise 2.
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean.flatMap(
      m =>
        if (xs.isEmpty) None
        else Some(
          xs.foldLeft
            (0.0)
            ((accumulator, value) => accumulator + math.pow(value - m, 2))
            / xs.length
        )
    )
  }
}
