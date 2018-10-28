package chapter5

object StrictAndNonStrict {
  sealed trait Stream[+A] {

    import Stream._

    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, _) => Some(h())
    }
    def toList: List[A] = this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }
    def toListTailRec: List[A] = {
      @annotation.tailrec
      def go(acc: List[A], stream: Stream[A]): List[A] = stream match {
        case Empty      => acc
        case Cons(h, t) => go(acc :+ h(), t())
      }
      go(Nil, this)
    }
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if (n > 1)  => cons(h(), t().take(n - 1))
      case Cons(h, _) if (n == 1) => cons(h(), empty)
      case _                      => empty
    }
    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0  => t().drop(n - 1)
      case _ => this // n == 0, n < 0, this == empty, stream has empty tail
    }
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }
    def takeWhileTailRec(p: A => Boolean): Stream[A] = {
      @annotation.tailrec
      def go(p: A => Boolean, acc: Stream[A]): Stream[A] = acc match{
        case Cons(h, t) if (p(h())) => go(p, cons(h(), t().takeWhile(p)))
        case _                      => Empty
      }
      go(p, this)
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }
}