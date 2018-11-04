package other

object BinarySearchAlgorithm {

  def find(n: Int, list: List[Int]): Boolean = {
    if (list.isEmpty)
      false
    else {
      val pivot = list(list.size / 2)
      if (n < pivot) find(n, list.slice(0, list.size / 2))
      else if (n > pivot) find(n, list.slice(list.size / 2 + 1, list.size))
      else true
    }
  }
}