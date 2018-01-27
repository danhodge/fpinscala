object Exercise1 {
  // exercise 2.1 - tail recursive fibonacci
  def fib(n: Int): Int = {
    def go1(n: Int): Int =
      if (n == 0) 0
      else if (n == 1) 1
      else go2(n, 0, 1)

    def go2(n: Int, n2: Int, n1: Int): Int =
      if (n == 2) n2 + n1
      else go2(n-1, n1, n1 + n2)

    go1(n)
  }

  def main(args: Array[String]): Unit =
    println(fib(9))
}
