package example

object AList {

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of Empty List");
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of Empty List")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = ys match {
    case List() => xs;
    case z::zs => z::concat(xs, zs)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y::ys => reverse(ys) ::: List(y)
  }

  def removeAt[T](index: Int, xs: List[T]): List[T] = xs.take(index) ::: xs.drop(index+1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case List(y:List[Any],z:List[Any]) => flatten(y) ::: flatten(z)
    case y :: ys => y :: flatten(ys)
  }

}
