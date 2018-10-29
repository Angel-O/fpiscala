package other

object BinarySearchTree {

  case class IntBst(data: Int, left: Option[IntBst] = None, right: Option[IntBst] = None) {

    def findValue(value: Int): Boolean = value match {
      case `data` => true
      case _ => (for {
        leftTree <- left
        rightTree <- right
      } yield (leftTree.findValue(value) || rightTree.findValue(value))).getOrElse(false)
    }

    /*
     * value < data => insert on left
     * 	sub-left == null => sub-left == Tree(value) otherwise call insert on sub-left
     * value > data => insert on right
     * 	sub-right == null => sub-rigth == Tree(value) otherwise call insert on sub-right
     * */
//    def insert(value: Int): IntBst = {
//
//      //@annotation.tailrec
//      def go(value: Int, currentTree: IntBst): IntBst = {
//        if (value == currentTree.data) this
//        else if (value < currentTree.data) {
////          currentTree.left
////          .fold(currentTree.copy(left = Some(IntBst(value))))(go(value, _))
//          currentTree.left match {
//            case None => currentTree.copy(left = Some(IntBst(value)))
//            case Some(t) => go(value, t)
//          }
//        }
//        else {
//          currentTree.right
//          .fold(currentTree.copy(right = Some(IntBst(value))))(go(value, _))
//        }
//      }
//
//      go(value, this)
//    }
    
    def insert(value: Int): IntBst = {

      //@annotation.tailrec
      def doInsert(currentTree: Option[IntBst]): IntBst = currentTree match {
        case None => IntBst(value)
        case Some(t) => {
          if (value > t.data) t.copy(right = Some(doInsert(t.right)))
          else if (value < t.data) t.copy(left = Some(doInsert(t.left)))
          else t
        }
      }
      
      def doInsert2(currentTree: Option[IntBst]): IntBst = {
        currentTree.fold(IntBst(value))(t => value match {
          case v if (v > t.data) => t.copy(right = Some(doInsert2(t.right)))
          case v if (v < t.data) => t.copy(left = Some(doInsert2(t.left)))
          case _ => t
        })
      }

      doInsert(Some(this))
    }

    def inOrder(f: Int => Unit): Unit = {
      def go(f: Int => Unit, currentTree: IntBst): Unit = {

        currentTree.left.fold()(go(f, _))
        f(currentTree.data)
        currentTree.right.fold()(go(f, _))
      }

      go(f, this)
    }

    def preOrder(f: Int => Unit): Unit = {
      def go(f: Int => Unit, currentTree: IntBst): Unit = {

        f(currentTree.data)
        currentTree.left.fold()(go(f, _))
        currentTree.right.fold()(go(f, _))
      }

      go(f, this)
    }

    def postOrder(f: Int => Unit): Unit = {
      def go(f: Int => Unit, currentTree: IntBst): Unit = {

        currentTree.left.fold()(go(f, _))
        currentTree.right.fold()(go(f, _))
        f(currentTree.data)
      }

      go(f, this)
    }
  }
}