type Set = Int => Boolean

//a
def createEmptySet:Set =
 _ => false

//b
def contains(elem:Int, set:Set):Boolean =
  set(elem)

//c
def insert(elem:Int, set:Set):Set =
 x=> x== elem || set(elem)