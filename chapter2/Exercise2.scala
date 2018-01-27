object Exercise2 {
  // exercise 2.2 - polymorphic isSorted function
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false

    loop(1)
  }

  private def stringCmp(s1: String, s2: String): Boolean = {
    s1.compareTo(s2) <= 0;
  }

  def main(args: Array[String]): Unit =
    println(isSorted(args, stringCmp))
}
