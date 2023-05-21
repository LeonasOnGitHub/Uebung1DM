//Aufgabe Folien Filter Uebung
def filter(l: List[Int], pred: Int => Boolean): List[Int] = l match {
  case Nil => Nil
  case x :: xs =>
    if (pred(x)) x :: filter(xs, pred)
    else filter(xs, pred)
}

//Aufgabe Folien Aggregations Function
def fold(i: List[Int], base: Int, aggFun: (Int, Int) => Int): Int = {
  if (i.isEmpty) base
  else (fold(i.tail, base + i.head, aggFun))
}
fold(List(1, 3, 6, 9), 0, (x, y) => x + y)

//Aufgabe 1
type Set = Int => Boolean
val func = (x: Int) => (x % 2 == 0)
def gradeZahlen(): Set =
  (x: Int) => (x % 2 == 0)

//Aufgabe a
def createEmptySet(): Set =
  _ => false //try x

//Aufgabe b
def contains(s: Set, el: Int): Boolean =
  s(el)

//Aufgabe c
def insert(elem: Int, set: Set): Set =
  x => x == elem || set(x)


//Augabe d rek
def createRangeRek(a: Int, b: Int): Set = {
  if (a > b) createEmptySet()
  else insert(a, createRangeRek(a + 1, b))
}
//Augabe d
def createRange(a: Int, b: Int): Set =
  x=> x>=a && x<=b

//Aufgabe e
def union(set1: Set, set2: Set): Set =
