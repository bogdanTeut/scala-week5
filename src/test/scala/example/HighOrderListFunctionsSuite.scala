package example

import example.HighOrderListFunctions._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner;

@RunWith(classOf[JUnitRunner])
class HighOrderListFunctionsSuite extends FunSuite {

  test("square list") {
    assert(squareList(List(1,2,3,4,5)) === List(1,4,9,16,25))
    assert(squareListV2(List(1,2,3,4,5)) === List(1,4,9,16,25))
  }

  test("pos elements") {
    assert(posElems(List(-1,0,3,4)) === List(3,4))
    assert(posElemsV2(List(-1,0,3,4)) === List(3,4))
  }

  test("pack") {
    assert(pack(List("a","a","a","b","c","c","a")) === List(List("a","a","a"), List("b"), List("c","c"), List("a")))
  }

  test("encode") {
    assert(encode(List("a","a","a","b","c","c","a")) === List(("a", 3), ("b", 1), ("c", 2), ("a", 1)))
  }

  test("sum") {
    assert(sum(List(1,2,3,4)) === 10)
    assert(sum(List()) === 0)
    assert(sumV2(List(1,2,3,4)) === 10)
    assert(sumV2(List()) === 0)
    assert(sumV3(List(1,2,3,4)) === 10)
    assert(sumV3(List()) === 0)
    assert(sumV4(List(1,2,3,4)) === 10)
    assert(sumV4(List()) === 0)
    assert(sumV5(List(1,2,3,4)) === 10)
    assert(sumV5(List()) === 0)
    assert(sumV6(List(1,2,3,4)) === 10)
    assert(sumV6(List()) === 0)
  }

  test("prod") {
    assert(prod(List(1,2,3,4)) === 24)
    assert(prod(List()) === 1)
    assert(prodV2(List(1,2,3,4)) === 24)
    assert(prodV2(List()) === 1)
    assert(prodV3(List(1,2,3,4)) === 24)
    assert(prodV3(List()) === 1)
    assert(prodV4(List(1,2,3,4)) === 24)
    assert(prodV4(List()) === 1)
    assert(prodV5(List(1,2,3,4)) === 24)
    assert(prodV5(List()) === 1)
    assert(prodV6(List(1,2,3,4)) === 24)
    assert(prodV6(List()) === 1)
  }

  test("concat") {
    assert(concat(List(1,2,3,4), List(5,6,7,8)) === List(1,2,3,4,5,6,7,8))
  }
}
