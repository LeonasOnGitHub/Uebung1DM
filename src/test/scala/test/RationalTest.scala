package test

import org.scalatest.funsuite.AnyFunSuite
import uebung1.Rational

class RationalTest extends AnyFunSuite {
  test("Rational Inititalisation 1") {
    val x = new Rational(1, 2)
    assert(x.value === 0.5)
  }

  test("Rational add") {
    val x = new Rational(1, 2)
    assertResult("7/6") {
      x.add(new Rational(2, 3)).toString
    }
  }

  test("Rational neg") {
    val x = new Rational(4, 2)
    assertResult("-2/1") {
      x.neg.toString
    }
  }

  test("Rational sub") {
    val x = new Rational(4, 2)
    assertResult("1/2") {
      x.sub(new Rational(3,2)).toString
    }
  }

  test("Rational multi") {
    val x = new Rational(4, 2)
    assertResult("1/1") {
      x.multi(new Rational(2,4)).toString
    }
  }

  test("Rational Inititalisation 2") {
    val x = new Rational(1, 2)
    assertResult("1/2") {
      x.toString
    }
  }

  test("Test requirement (denominator!=0)") {
    intercept[IllegalArgumentException] {
      new Rational(1, 0)
    }
  }
}
