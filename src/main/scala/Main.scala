import three.{Branch, Leaf, Tree}

object Main {
  def main(args: Array[String]): Unit =
    {
      val tree = Branch(Leaf(1), Branch(Branch(Leaf(3), Branch(Leaf(3), Leaf(4))), Branch(Leaf(3), Leaf(4))))

      println(Tree.foldMax(tree))
    }
}
