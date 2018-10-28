package chapter3

object RecursionOverLists {
  
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case h :: t => foldLeft(t, f(z, h))(f) 
    }
  }
  
  def sumWithFoldLeft(as: List[Int]): Int = {
    as match {
      case Nil => 0
      case h :: t => foldLeft(t, h)(_ + _)
    }
  }
  
  def sumWithFoldLeftBetter(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  
  def productWithFildLeft(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
}