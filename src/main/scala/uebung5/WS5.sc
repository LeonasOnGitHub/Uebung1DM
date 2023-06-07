import scala.collection.mutable

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
  x=> x== set1(x) || set2(x)



// Aufgaben Vorlesung Aggregar funktionen
val l =List(1,2,3,4,5)
l.foldLeft("")((a,b)=>a+b)
l.foldRight("")((a,b)=>a+b)

l.reduceRight((a,b)=>a-b)
l.reduceLeft((a,b)=>a-b)

val a = Array(1,2,"s",3)

val v1= Array(1,2,3)
val v2=Array(4,5,6)
v1.zip(v2).map(x=> x._1*x._2)


//GroupBY
//1
val l =  List(1,2,3,4,5,6)
def groupBY[T,u](l:Iterable[T], groupByFun:T=>u):mutable.Map[u, List[T]]={
 val res = mutable.Map[u,List[T]]()
  for (el <-l){
    res += (groupByFun(el) -> List(el))
  }
  res
}
l.groupBy(x=>x%2)
groupBY(l, (x:Int)=>x % 2)

def groupBYMPL[T,u](l:Iterable[T])( groupByFun:T=>u):mutable.Map[u, List[T]]={
  val res = mutable.Map[u,List[T]]()
  for (el <-l){
    res += (groupByFun(el) -> List(el))
  }
  res
}
groupBYMPL(l)(x=>x % 2)

//Aufgabe 2
def groupBY2[T,u](l:Iterable[T], groupByFun:T=>u):Map[u, List[T]]={
  var res = Map[u,List[T]]()
  for (el <-l){
    res = res.updated(groupByFun(el), el::res.getOrElse(groupByFun(el), List()))
  }
  res
}
l.groupBy(x=>x%2)
groupBY2(l, (x:Int)=>x % 2)

val l =  List(1,2,3,4,5,6)
l.reduceLeft((x, y) => x + y)