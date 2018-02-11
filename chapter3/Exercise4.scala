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

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) {
      l
    } else {
      l match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Exercise4 {
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4, 5)
    val t = List.drop(l, 5)
    println(t)
  }
}
