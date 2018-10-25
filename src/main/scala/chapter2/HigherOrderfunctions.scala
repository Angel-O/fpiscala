package chapter2

import scala.reflect.ClassTag

object HigherOrderfunctions {
  def isSorted[A](items: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if(items.length < 2){
      true
    }
    else{
      val Array(first, second, _*) = items
      if(ordered(first, second)) {
       isSorted(items.drop(2), ordered)
       }
       else false
    }  
  }
  
  // no class tag
  def isSortedPatternOnLength[A](items: Array[A], ordered: (A, A) => Boolean): Boolean = {
    items.length match {
      case 0 | 1 => true
      case _ => {
        val tail = items.tail
        ordered(items.head, tail.head) && isSorted(tail.tail, ordered)
      }
    }
  }
  
  // class tag required
  def isSortedPatternOnStructure[A: ClassTag](items: Array[A], ordered: (A, A) => Boolean): Boolean = {
    items match {
      case Array(_) | Array() => true
      case Array(first, second, rest @ _*) => {
        ordered(first, second) && isSorted(rest.toArray, ordered)
      }
    }
  }
}