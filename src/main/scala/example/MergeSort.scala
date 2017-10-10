package example

object MergeSort {

//  def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
//    case Nil => ys
//    case x :: xs1 => ys match {
//      case Nil => xs
//      case y :: ys1 => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
//    }
//  }

  def mergeSort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    list match {
      case Nil => list
      case x :: Nil => list
      case x :: xs => {
        val (first, second) = list.splitAt(list.size/2)
        merge(mergeSort(first), mergeSort(second))
      }
    }
  }

}
