package uebung1

class Rational(val numerator: Int, val denominator: Int) {
  def neg: Rational = {
    new Rational(-num, denom)
  }

  private val g = gcd(numerator, denominator)


  def this(denom: Int) = this(1, denom)

  override def toString: String = s"$num/$denom"

  require(denominator != 0, "Denominator mmuss != 0 sein")

  def num: Int = numerator/g

  def denom: Int = denominator/g

  def value: Double = (num.toDouble / denom)

  def max(x: Rational): Rational = {

    if (numerator / denominator < x.num / x.denom) this else x
  }

  def add(x: Rational): Rational = {
    val newNum = num * x.denom + denom * x.num
    val newDenum = denom * x.denom
    new Rational(newNum, newDenum)

  }

  def sub(x: Rational): Rational = {
    add(x.neg)
  }

  def multi(x: Rational): Rational = {
    val newNum = num * x.num
    val newDenum = denom * x.denom
    new Rational(newNum, newDenum)
  }

  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }
}

