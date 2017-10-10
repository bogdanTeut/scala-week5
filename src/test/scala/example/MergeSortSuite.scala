package example

import example.MergeSort._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner;

@RunWith(classOf[JUnitRunner])
class MergeSortSuite extends FunSuite {

  test("mergeSort for one element list") {
    assert(mergeSort(List(1)) === List(1))
  }

  test("mergeSort for empty list") {
    assert(mergeSort(List()) === List())
  }

  test("mergeSort for two elements list") {
    assert(mergeSort(List(2,1)) === List(1,2))
  }

  test("mergeSort for four elements list") {
    assert(mergeSort(List(2,1,3,-1)) === List(-1,1,2,3))
  }
}
