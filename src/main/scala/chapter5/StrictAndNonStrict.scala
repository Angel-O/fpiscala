package chapter5

object StrictAndNonStrict {
  sealed trait Stream[+A] {

    import Stream._

    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, _) => Some(h())
    }
    def tail: Stream[A] = this drop 1
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
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
    def forAllWithFoldRight(p: A => Boolean): Boolean = {
      foldRight(true)((curr, acc) => (p(curr) && acc))
    }
    def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((curr, acc) => if(p(curr)) cons(curr, acc) else acc)
    }
    def headOptionWithFoldRight: Option[A] = {
      foldRight(None: Option[A])((curr, acc) => this match {
        case Cons(h, _) => Some(h())
        case _          => acc
      })
    }
    def headOptionWithFoldRightBetter: Option[A] = {
      foldRight(None: Option[A])((h, _) => Some(h))
    }
    
    def zip[B](other: Stream[B]): Stream[(A, B)] = (this, other) match{
      case (Cons(th, tt), Cons(oh, ot)) => cons((th(), oh()), tt() zip ot ())
      case _ => Empty
    }
    
    def map[B](f: A => B): Stream[B] = this match {
      case Cons(h, t) => cons(f(h()), t().map(f))
      case Empty => Empty
    }
  }
  case object Empty extends Stream[Nothing]
  
  // adding '-thunk' suffix because that's what they are and to avoid conflicts with tail in Stream trait
  case class Cons[A](headThunk: () => A, tailThunk: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      // adding 'lazy-' prefix because that's what they are and to avoid conflicts with tail in Stream trait
      lazy val lazyHead = hd
      lazy val lazyTail = tl
      Cons(() => lazyHead, () => lazyTail)
    }
    
    implicit class toStreamHead[A](h: A){
      def #::(other: Stream[_]): Stream[_] = cons(h, other)
      def #::(other: A): Stream[A] = Stream(h, other)
    }
    
    implicit class toStreamTail[A](stream: Stream[A]){
      def #::(tail: Stream[A]): Stream[A] = stream match {
        case Cons(h, t) => Cons(h, () => t() #:: tail)
        case _ => tail
      }
      def #::(last: A): Stream[A] = stream match {
        case Empty => cons(last, stream)
        case _ => stream #:: Stream(last)
      }
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }
}