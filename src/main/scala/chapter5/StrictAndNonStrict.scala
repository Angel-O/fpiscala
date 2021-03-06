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
      case (Cons(th, tt), Cons(oh, ot)) => cons((th(), oh()), tt() zip ot())
      case _ => Empty
    }
    
    def map[B](f: A => B): Stream[B] = this match {
      case Cons(h, t) => cons(f(h()), t().map(f))
      case Empty => Empty
    }
    
    import StreamsPlayGround.unfold
    
    def mapWithUnfold[B](f: A => B): Stream[B] = 
      unfold(this){ as => as.headOption.map(head => (f(head), as.tail)) }

    def takeWithUnfold(n: Int): Stream[A] = {
      if (n >= 1)
        unfold(this) { as => as.headOption.map(head => (head, as.tail.takeWithUnfold(n - 1))) }
      else
        empty
    }
    
    // one liner and not recursive in takeWithUnfold
    def takeWithUnfoldOneLiner(n: Int): Stream[A] = {
        unfold((this, n)) { case (as, i) => as.headOption.collect { case head if(i >= 1) => (head, (as.tail, i - 1)) } }
    }

    def takeWhileWithUnfold(n: Int)(f: A => Boolean): Stream[A] =
      if (n >= 1)
        unfold(this)(as =>
          as match {
            case Cons(h, t) if (f(h())) => Some(h(), t())
            case _          => None
          })
      else empty
      
    def takeWhileWithUnfoldOneLiner(n: Int)(f: A => Boolean): Stream[A] =
      unfold(this)( as => as.headOption.collect { case head if(n > 0 && f(head)) => (head, as.tail) } )

    // initialState => this stream, that stream
    // nextElement => head of this, head of that
    // nextState => tail of this, tail of that
    // Scala is amazing !!!
    def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, that)) {
        case (a, b) => (a, b) match {

          case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
          case (empty, Cons(h, t))          => Some(((None, Some(h())), (empty, t())))
          case (Cons(h, t), empty)          => Some(((Some(h()), None), (t(), empty)))
          case _                            => None // or equivalently... Some(((None, None), (empty, empty)))

        }
      }
    }
    
    // stack overflow with infinite streams!!!
    def startsWith[A](that: Stream[A]): Boolean = {
      this zip that map { case (a, b) => a == b } forAll identity
    }
    
    def startsWith2[A](that: Stream[A]): Boolean = {
      this.zipAll(that).takeWhile(!_._2.isEmpty) forAll { case (a, b) => a == b }
    }
    
    // initial state => this
    // nextElement => currentStream
    // nextState => currentStream.tail
    def tails: Stream[Stream[A]] = {
      unfold(this)( _ match { case Empty => None case stream @ Cons(_, t) => Some((stream, t())) })
    }

    // inital state => this that
    // nextElement => f(tuple(thisHead, thatHead))
    // nextState => tuple(thisTail, thatTail)
    def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] = {
      unfold(this, that) {
        _ match {
          case (Cons(ah, at), Cons(bh, bt)) => Some { ( f(ah(), bh()), (at(), bt()) ) }
          case _                            => None
        }
      }
    }
  }
  case object Empty extends Stream[Nothing]
  
  // adding '-thunk' suffix because that's what they are and to avoid name conflicts with tail defined in Stream trait
  case class Cons[A](headThunk: () => A, tailThunk: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      // adding 'lazy-' prefix because that's what they are and to avoid name conflicts with tail defined in Stream trait
      lazy val lazyHead = hd
      lazy val lazyTail = tl
      Cons(() => lazyHead, () => lazyTail)
    }
    
    // note the parameter is passed by name to avoid the evaluation of the stream
    // that woul cause a stack-overflow error
    implicit class streamAppender[A](tail: => Stream[A]){
      def #::(h: A): Stream[A] = cons(h, tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }
}