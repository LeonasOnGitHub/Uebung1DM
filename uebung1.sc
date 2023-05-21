def square(x: Int): Int = x * x
square(8)

//CBN und CBV Aufgabe 1
def or(x: Boolean, y: => Boolean): Boolean = {
  if (x || y) {
    true
  } else {
    false
  }
}

def loop(): Boolean = {
  loop()
}
or(true, loop)

//Aufgabe 2
def myMethod(param: Int): String = {
  if (param < 0) "kleiner null"
  else if (param > 0) "größer null"
  else "null"
}
myMethod(2)
myMethod(-2)
myMethod(0)
myMethod(2999)

//Aufgabe 4
def squareUnder(x: Double, max: Double): Double = {
  if (x * x > max) x
  else squareUnder(x * x, max)
}
squareUnder(2, 200)

//Aufgabe 5
//a
def teiler(x: Int): Int = {
  def counter(y: Int): Int = {
    if (x % y == 0) y
    else counter(y - 1)
  }

  counter(x - 1)
}
teiler(900)

//b
def teiler(x: Int): Int = {
  def counter(y: Int): Int = {
    def countHelper(z: Int): Int = {
      if (z>=x) y
      else if (x % z == 0) z
      else countHelper(z + 1)
    }
    if (x % y == 0) countHelper(y + 1)
    else counter(y + 1)
  }

  counter(1)
}
teiler(900)

def sumLoop(x: Int): Int ={
  if (x<=0) 0
  else if (x%5==0 || x%3==0) x+sumLoop(x-1)
  else sumLoop(x-1)
}
sumLoop(10)

/*def kPrim(n:Int):Long={
  def isPrim(x:Int):Boolean={
    def isPrimHelper(c:Int):Boolean={
      if (c==x)true
      else if (x%c==0)false
      else isPrimHelper(c+1)
    }
    if (x==1) false else isPrimHelper(2)
  }
if
}

 */

def fiboDown (x:Int):BigInt={
  if (x==0) 0
  else if (x==1) 1
  else fiboDown(x-1) + fiboDown(x-2)
}
fiboDown(10)

/*def fiboUp(x:Int):BigInt={
 def fiboHelper(z:Int):BigInt= {
   if (z==0)
 }

}
 */