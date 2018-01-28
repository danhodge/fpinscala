object Exercise3 {
  // exercise 2.3 - currying
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def main(args: Array[String]): Unit = {
    val f = (a: Int, b: Int) => a + b
    val g = curry(f)(2)
    println(g(3))
  }
}
