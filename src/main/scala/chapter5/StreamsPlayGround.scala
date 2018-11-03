package chapter5

object StreamsPlayGround {

  import StrictAndNonStrict._
  import StrictAndNonStrict.Stream._

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def nthFibonacci(nth: Int): Int = nth match {
    case 0 => 0
    case 1 => 1
    case _ => nthFibonacci(nth - 1) + nthFibonacci(nth - 2)
  }

  def generateSequence(start: Int, f: Int => Int): Stream[Int] = cons(f(start), generateSequence(start + 1, f))

  // the whole fibonacci sequence: note how the
  // function takes no parameters, meaning there is no need to specify a limit.
  // Note: since it's an infinite stream use stream.take(n).toList to inspect it
  def fibs: Stream[Int] = generateSequence(0, nthFibonacci)

  def fibsShort: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }
  
  def fibsSmart: Stream[Int] = cons(0, cons(1, fibsSmart zip (fibsSmart drop 1) map { case (a, b) => a + b }))
  
  // recreating doc example via implicits (https://www.scala-lang.org/api/2.12.3/scala/collection/immutable/Stream.html)
  def fibsNeat: Stream[Int] = 0 #:: 1 #:: (fibsNeat zip (fibsNeat.tail) map { case (a, b) => a + b })

  // unfold with option.map
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(pair => cons(pair._1, unfold(pair._2)(f))).getOrElse(empty)
  }

  // unfold with option.fold (note how type [A] on empty needs to be specified...drawback of option.fold)
  def unfoldUnreadable[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold(empty[A])({ case (a, s) => cons(a, unfoldUnreadable(s)(f)) })
  }

  // unfold with pattern matching and long variable names
  def unfoldReadable[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None                           => empty
      case Some((nextElement, nextState)) => cons(nextElement, unfoldReadable(nextState)(f))
    }
  }

  //TODO test this one!!!
  def fibsWithUnfold: Stream[Int] = {

    // the state represents the next start element
    def getNextFib(a: Int): Option[(Int, Int)] = Some((nthFibonacci(a), a + 1))

    unfold(0)(n => getNextFib(n))
  }
  
  def fibsWithUnfold2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (a, a + b))) }
  
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))
  
  def constantWithUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some((n, n)))
}