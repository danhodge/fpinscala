sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](as: List[A]): List[A] = as match {
    case Nil => as
    case Cons(x, xs) => foldLeft(xs, List(x))((rl, a) => Cons(a, rl))
  }
}

object Exercise12 {
  def main(args: Array[String]): Unit = {
    println(List.reverse(List(1, 2, 3, 4, 5)))
    println(List.reverse(List(1)))
    println(List.reverse(Nil))
  }
}
