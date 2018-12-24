package chapter7

import java.util.concurrent._
//import java.util.concurrent.TimeUnit

object FunctionalParallelism {

//  class ExecutorService {
//    def submit[A](a: Callable[A]): Future[A]
//  }
//  trait Callable[A] { def call: A }

//  trait Future[A] {
//    def get: A
//    def get(timeout: Long, unit: TimeUnit): A
//    def isCancelled: Boolean
//    def cancel(evenIfRunning: Boolean): Boolean
//  }

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
    
//    def map2WithTimeout[A, B, C](a: Par[A], b: Par[B], timeout: Long, units: TimeUnit)(f: (A, B) => C): Par[C] =
//      (es: ExecutorService) => {
//        val af = a(es)
//        val bf = b(es)
//        UnitFuture(f(af.get(timeout, units), bf.get(timeout, units)))
//      }

    def fork[A](a: => Par[A]): Par[A] = {
      es =>
        es.submit(new Callable[A] {
          def call = a(es).get
        })
    }
    
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s) 
    
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
    
    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
      map2(parList, unit(Unit))((a, _) => a.sorted)
    }
    
    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(Unit))((a, _) => f(a))
    }
    
    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
    
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldLeft(unit(List[A]()))((acc, cur) => {
        es => UnitFuture(cur(es).get :: acc(es).get)
      })
    }
    
    def sequenceViaFoldLeft[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldLeft(unit(List[A]()))((acc, cur) => {
        es => UnitFuture(cur(es).get :: acc(es).get) //this is exactly what map2 does... see sequenceViaFoldRight
      })
    }
    
    def sequenceViaFoldRight[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(List[A]()))((cur, acc) => map2(cur, acc)(_ :: _))
    }
   
    // yeah, but that's not multiple filtering in 
    // parallel, it's just filtering once in parallel
    def parFilterBad[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      sequence(as.filter(f).map(unit)) 
    }
    
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars = as.map(asyncF({ case a if f(a) => List(a) case _ => Nil }))
      map(sequence(pars))(_.flatten) // plain english: clear !!!
    }
    
    def countPar(paragraphs: List[String]) = {
      var pars = paragraphs.map(asyncF({ case p => p.split(" ").size })).toIndexedSeq
      var (a, b) = pars.splitAt(pars.size / 2)
      map2(sequence(a.toList), sequence(b.toList))(_.sum + _.sum)
    }
    
    val splitWords = (s: String) => s.split(" ")
    val countWords = (s: Array[_]) => s.size
    
    def countParNeat(paragraphs: List[String]) = { 
      var pars = paragraphs.map(asyncF(splitWords andThen countWords)).toIndexedSeq
      var (a, b) = pars.splitAt(pars.size / 2)
      map2(sequence(a.toList), sequence(b.toList))(_.sum + _.sum)
    }
    
    def countParNeatGood(paragraphs: List[String]) = {
      var pars = paragraphs.map(asyncF(splitWords andThen countWords))
      map(sequenceViaFoldRight(pars))(_.sum)
    }
  }
}