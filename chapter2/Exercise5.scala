object Exercise5 {
  // exercise 2.5 - composition
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    val f = (a: Int) => a + 1
    val g = (a: Int) => a * 3

    println(compose(f, g)(6))
  }
}
