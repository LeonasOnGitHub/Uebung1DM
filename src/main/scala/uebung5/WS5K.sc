type Set = Int => Boolean

//a
def createEmptySet: Set =
  _ => false

//b
def contains(elem: Int, set: Set): Boolean =
  set(elem)

//c
def insert(elem: Int, set: Set): Set =
  x => x == elem || set(elem)

//d
def createRange(a: Int, b: Int): Set =
  x => x > a && x < b

//e
def union(set1: Set, set2: Set): Set =
  x => contains(x, set1) || contains(x, set2)

//f
def toList(set: Set, a: Int, b: Int): List[Int] = {
  if (a > b) List()
  else if (contains(a, set)) a :: toList(set, a + 1, b)
  else toList(set, a + 1, b)
}
