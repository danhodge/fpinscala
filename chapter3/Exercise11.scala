sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length[A](as: List[A]) = foldLeft(as, 0)((len, _) => len + 1)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
}

object Exercise11 {
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4, 5)
    println(List.sum(l))
    println(List.product(List(1.0, 2.0, 3.0, 4.0, 5.0)))
    println(List.length(l))
  }
}
