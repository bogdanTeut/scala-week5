package example

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import AList._;

@RunWith(classOf[JUnitRunner])
class AListSuite extends FunSuite {

  test("Last for empty list throws empty List Error") {
    val caught = intercept[Error] {
      last(List())
    }
    assert(caught.getMessage === "last of Empty List")
  }

  test("Last for list of one elem returns it") {
    assert(last(List(1)) === 1)
  }

  test("last for list") {
    assert(last(List(1,2,3,4)) === 4)
  }

  test("init for empty list throws empty list error") {
    def caught = intercept[Error] {
      init(List())
    }
    assert(caught.getMessage === "init of Empty List")
  }

  test("init for one element list") {
    assert(init(List(1)) === List())
  }

  test("init for list") {
    assert(init(List(1,2,3,4)) === List(1,2,3))
  }

  test("concat for empty list return the non empty list") {
    assert(concat(List(1,2,3), List()) === List(1,2,3))
  }

  test("concat for two non empty lists") {
    assert(concat(List(4,5,6),  List(1,2,3)) === List(1,2,3,4,5,6))
  }

  test("reverse for empty list") {
    assert(reverse(List()) === List())
    //assert(reverse(List(1,2,3,4)) === List(4,3,2,1))
  }

  test("reverse for list") {
    assert(reverse(List(1,2,3,4)) === List(4,3,2,1))
  }

  test("removeAt") {
    assert(removeAt(1, List('a','b','c','d')) === List('a', 'c', 'd'))
  }

  test("flatten for empty list") {
    assert(flatten(List()) === List())
  }

  test("flatten for a list") {
    assert(flatten(List(1,2,3,4)) === List(1,2,3,4))
  }

  test("flatten for a list of lists") {
    assert(flatten(List(List(1,2), List(3,4))) === List(1,2,3,4))
  }
}
