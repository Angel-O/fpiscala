package chapter8

import chapter6.StatefulApi._
import chapter6.StatefulApi.State

object PropertyBasedTesting {
  case class Gen[A](sample: State[RNG, A]){
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      val mapped = sample.map(f)
      Gen(State.map2(sample, mapped)((a, genB) => genB.sample.run(a) match { case (s, _) => s }))

      // or
      Gen(sample.flatMap(a => f(a).sample))
    }

    // Not clear .... at all...
    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size flatMap(v => Gen.listOfN(v, this))
    }
  }

  object Gen{
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(nonNegativeInt).map(n => start + n % (stopExclusive-start))) // Ok...
    }

    def unit[A](a: => A): Gen[A] = {
      Gen(State.unit(a))
    }

    // generates booleans at random
    def boolean: Gen[Boolean] = {
      Gen(State(nonNegativeInt).map(n => n % 2 == 0))
    }

    //generates lists of length n using the generator g
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      Gen(State.sequence(List.fill(n)(g.sample)))
    }

    def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
      Gen(listOfN(2, choose(start, stopExclusive)).sample.map { case a :: b :: _ => (a, b) })
    }

    def toOption[A](g: Gen[A]): Gen[Option[A]] = {
      Gen(g.sample.map(a => Option(a)))
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      boolean.flatMap(b => if (b) g1 else g2)
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {



      // this will always generate the same stuff...
      (g1, g2) match {
        case ((genA, a), (_, b)) if (a > b) => genA
        case ((_, a), (genB, b)) if (a < b) => genB
        case ((genA, a), (genB, b)) if (a == b) => union(genA, genB)
      }

      // this is the correct approach... TODO fill in the gaps
      val probability: Double = ???
      Gen(State(nonNegativeInt)).flatMap(v => if (v > probability) ??? else ???)
    }
  }
}
