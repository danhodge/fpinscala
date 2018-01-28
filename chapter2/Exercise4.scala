object Exercise4 {
  // exercise 2.4 - uncurrying
  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // from exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def main(args: Array[String]): Unit = {
    val f = (a: Int, b: Int) => a + b
    val g = uncurry(curry(f))

    println(g(5, 6))
  }
}
