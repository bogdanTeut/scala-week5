package example

object HighOrderListFunctions {

  def squareList(list: List[Int]): List[Int] = list match {
    case Nil => list
    case x :: xs => x * x :: squareList(xs)
  }

  def squareListV2(list: List[Int]): List[Int] = list map (x => x * x)

  def posElems(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case x :: xs => if (x > 0) x :: posElems(xs) else posElems(xs)
  }

  def posElemsV2(list: List[Int]): List[Int] = list filter (_ > 0)

  def pack(list: List[String]): List[List[String]] = list match {
    case Nil => Nil
    case x :: xs => {
      val (first, last) = xs span (_.equals(x))
      (x :: first) :: pack(last)
    }
  }

  def encode(list: List[String]): List[(String, Int)] = pack(list).map(subList => (subList.head, subList.size))

  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

  def sumV2(list: List[Int]): Int = (0 :: list) reduceLeft (_ + _)

  def sumV3(list: List[Int]): Int = list.foldLeft(0)(_ + _)

  def foldLeft[T, U](list: List[T], z: U)(op:(U, T) => U) :U = list match {
    case Nil => z
    case x :: xs => xs.foldLeft(op(x, z))(op)
  }

  def reduceLeft(list: List[Int], op:(Int, Int) => Int): Int = list match {
    case Nil => throw new NoSuchElementException("reduce left fo empty list")
    case x :: xs => foldLeft(xs, x)(op)
  }

  def foldRight[T, U](list: List[T], z: U)(op:(T, U) => U) :U = list match {
    case Nil => z
    case x :: xs => op(x, foldRight(xs, z)(op))
  }

  def reduceRight(list: List[Int], op:(Int, Int) => Int): Int = list match {
    case Nil => throw new NoSuchElementException("reduce left fo empty list")
    case x :: Nil => x
    case x :: xs => op(x, reduceRight(xs, op))
  }

  def sumV4(list: List[Int]): Int = reduceLeft( 0 :: list, (_ + _))

  def sumV5(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

  def sumV6(list: List[Int]): Int = foldRight(list, 0)(_ + _)

  def sumV7(list: List[Int]): Int = reduceRight( 0 :: list, (_ + _))

  def prod(list: List[Int]): Int = list match {
    case Nil => 1
    case x :: xs => x * prod(xs)
  }

  def prodV2(list: List[Int]): Int = (1 :: list) reduceLeft (_ * _)

  def prodV3(list: List[Int]): Int = list.foldLeft(1)(_ * _)

  def prodV4(list: List[Int]): Int = reduceLeft( 1 :: list, (_ * _))

  def prodV5(list: List[Int]): Int = foldLeft(list, 1)(_ * _)

  def prodV6(list: List[Int]): Int = foldRight(list, 1)(_ * _)

  def prodV7(list: List[Int]): Int = reduceRight( 1 :: list, (_ * _))

  def concat(xs :List[Int], ys: List[Int]): List[Int] = foldRight(xs, ys)(_ :: _)

}
