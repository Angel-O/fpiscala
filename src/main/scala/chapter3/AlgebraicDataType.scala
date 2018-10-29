package chapter3

object AlgebraicDataType {
  sealed trait Tree[+A] {
//    def map0[B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
//      case Leaf(v)      => Leaf(f(v))
//      case Branch(l, r) => Branch(map0(l)(f), map0(r)(f))
//    }

    def size0[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size0(l) + size0(r)
    }

    def maximum0(tree: Tree[Int]): Int = tree match {
      case Leaf(v)      => v
      case Branch(l, r) => maximum0(l) max maximum0(r)
    }

    def depth0[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + (depth0(l) max depth0(r))
    }

    def map[B](f: A => B): Tree[B] = this match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(l map f, r map f)
    }

    def size: Int = this match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + l.size + r.size
    }

    def maximum: Int = this match {
      case Leaf(v: Int) => v
      case Branch(l, r) => l.maximum max r.maximum
    }

    def depth: Int = this match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + (l.depth max r.depth)
    }

//    def fold[B](z: B)(f: (Tree[A], Tree[A]) => B): B = this match {
//      case Leaf(v)      => z
//      case Branch(l, r) => f(l, r)
//    }
    def fold[B](z: A => B)(f: (Tree[A], Tree[A]) => B): B = this match {
      case Leaf(v)      => z(v)
      case Branch(l, r) => f(l, r)
    }

    def map1[B](f: A => B): Tree[B] = fold(lv => Leaf(f(lv)): Tree[B])((l, r) => Branch(l map1 f, r map1 f))

    def size1: Int = fold(_ => 0)((l, r) => 1 + l.size + r.size)

    def maximum1: Int = fold(lv => lv.asInstanceOf)((l, r) => l.maximum max r.maximum)

    def depth1: Int = fold(_ => 0)((l, r) => 1 + (l.depth max r.depth))
  }

  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}