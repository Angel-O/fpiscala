package chapter6

object StatefulApi {
  trait RNG{
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG{
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0xFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  
  def nonNegativeIntBad(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (n, g) if n == Int.MinValue => nonNegativeIntBad(g) // this is not referential transparent...
      case (n, g) if n < 0 => (n.abs, g)
      case (n, g) => (n, g)   
    }
  }
  
  // plus 1 to cover for n = Int.MinValue
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, g) = rng.nextInt 
    if (n < 0) (n.abs + 1, g) else (n, g)
  }
  
  // not good if n == 0
  // specs not met if n == 1
  def doubleBad(rng: RNG): (Double, RNG) = {
    val (n, g) = rng.nextInt
    (1/n.toDouble, g)
  }
  
  // plus 1 on the denominator to meet the specs
  def double(rng: RNG): (Double, RNG) = {
    val (n, g) = nonNegativeInt(rng)
    (n/(Int.MaxValue.toDouble + 1), g)
  }
  
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, g1) = rng.nextInt
    val (d, g2) = double(g1)
    ((i, d), g2)
  }
  
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), g) = intDouble(rng)
    ((d, i), g)
  }
  
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, g1) = double(rng)
    val (d2, g2) = double(g1)
    val (d3, g3) = double(g2)
    ((d1, d2, d3), g3)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {   
    (0 until count).foldLeft((List.empty[Int], rng))((acc, _) => {
      val (i, g) = acc._2.nextInt
      (i :: acc._1, g)
    })
  }
  
  // bad
  def intsRecBad(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (List(), rng)
    else {
      val (i, g) = rng.nextInt
      (i :: intsRecBad(count - 1)(g)._1, intsRecBad(count - 1)(g)._2)
    }
  }
  
  def intsRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG)  = {
      if(count == 0) (acc, rng)
      else {
        val (i, g) = rng.nextInt
        go(count - 1, i :: acc)(g)
      }
    }
    
    go(count, Nil)(rng)
  }
  
  
  type Rand[+A] = RNG => (A, RNG)
  
  val int: Rand[Int] = _.nextInt // the underscore here is an rng: remember Rand[A] is a function that takes an RNG
  
  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }
  
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
  
  def doubleNeat: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }
  
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
    // syntactic sugar for map2(ra, rb)((a, b) => (a, b))
  }
  
  def intDoubleNeat: Rand[(Int, Double)] = {
    both(int, double)
  }
  
  def doubleIntNeat: Rand[(Double, Int)] = {
    both(double, int)
  }
  
  def sequence[A](ts: List[Rand[A]]): Rand[List[A]] = {

    ts.foldLeft(unit(List.empty[A]))((acc, cur) => {
      rng => {
        val (s1, rng2) = both(acc, cur)(rng)
        (s1._2 :: s1._1, rng2)
      }
    })
  }
  
  def sequenceViaMap[A](ts: List[Rand[A]]): Rand[List[A]] = {

    ts.foldRight(unit(List.empty[A]))((cur, acc) => {
      map2(cur, acc)(_ :: _)
    })
  }
  
  def intsViaSequence(count: Int) = sequence(List.fill(count)(int))
  def intsViaSequence2(count: Int) = sequenceViaMap(List.fill(count)(int))
  
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n // 9 % 8 = 1 mod 1 => mod > 0 && i / n < 0 // come back to this... 
    if(i + (n-1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng)
  }
  
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { 
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
  
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => rng => (f(a), rng))
    //or flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    //flatMap(ra)(a => rng => (f(a, rb(rng)._1), rb(rng)._2))
    flatMap(ra)(a => rng => {
      val (b, rng2) = rb(rng)
      (f(a, b), rng2)
    })
    // or
    //flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
  
  //type State[S, +A] = S => (A, S)
  import State._
  case class State[S, A](run: S => (A, S)){
    
    def map[B](f: A => B): State[S, B] = {
      State(state => {
        val (a, state1) = this run state
        (f(a), state1)
      })
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      State(state =>
        {
          val (a, state1) = this run state
          g(a) run state1
        })
    }
    
    def mapViaFlatMap[B](f: A => B): State[S, B] = {
      flatMap(a => State(s => (f(a), s)))
    }
    
  }
  object State {
    type Rand[A] = State[RNG, A]
    
    def unit[S, A](a: A): State[S, A] = State(s => (a, s)) //useless
    
    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = {
      State(state => {
        val (a, state1) = sa run state
        val (b, state2) = sb run state1
        (f(a, b), state2)
      })
    }
    
    def sequence[S, A](ts: List[State[S, A]]): State[S, List[A]] = {

      def go(state: S, sts: List[State[S, A]], acc: List[A]): (List[A], S) = {
        sts match {
          case Nil => (acc, state)
          case h :: t => h run state match {
            case (a, state1) => go(state1, t, (a :: acc).reverse)
          }
        }
      }

      State(state => go(state, ts, List.empty))
    }
    
    def sequenceViaFoldRight[S, A](ts: List[State[S, A]]): State[S, List[A]] = {
   
      ts.foldRight(unit[S, List[A]](List.empty))((cur, acc) => map2(cur, acc)(_ :: _))
      
      //or ts.foldRight(unit[S, List[A]](List.empty))(map2(_, _)(_ :: _))
    }
    
    def sequenceViaFoldLeft[S, A](ts: List[State[S, A]]): State[S, List[A]] = {
   
      ts.foldLeft(unit[S, List[A]](List.empty))((acc, cur) => map2(acc, cur)(_ :+ _))
      
    }
    
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
    
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield () // shouldn't this return the modified state???
  }
  type RandViaState[A] = State[RNG, A]
}