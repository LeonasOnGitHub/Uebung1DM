//Aufgabe 1
val l1 = List(1, 2, 3, 4)
val l2 = List("a", "b", "c")

//a
val m = l1.foldLeft((0, 0, 0, 0))((m, e) => {
  if (e % 2 == 0) (m._1 + e, m._2 + 1, m._3, m._4)
  else (m._1, m._2, m._3 + e, m._4 + 1)
})
val avg = (m._1.toDouble / m._2, m._3.toDouble / m._4)

//test
l1.foldLeft(List.empty[Int])((a, b) => {
  b :: a
})

//b
l1.flatMap(x => List(x, x))

//c
l1.zip(l2)

//fold function
def fold[G, T](l: List[T], base: G, aggFun: (T, G) => G): G = l match {
  case Nil => base
  case x :: xs => aggFun(x, fold[G, T](xs, base, aggFun))
}

//Aufgabe2

//a
def moduloMap(l: List[Int], mod_value: Int): Map[Int, List[Int]] = {
  l.foldLeft(Map[Int, List[Int]]())((m, i) => m.updated(i % mod_value, i :: m.getOrElse(i % mod_value, List())))
}
def moduloMapGPT(l: List[Int], mod_value: Int): Map[Int, List[Int]] = {
  l.groupBy(_ % mod_value)
}
val l = List(1, 4, 5, 7, 8, 9)
moduloMap(l, 3)
moduloMapGPT(l, 3)

//b
def countLetters(l: List[String]): Map[Int, Int] = {
  l.foldLeft(Map[Int, Int]())((m, i) => m.updated(i.length, 1 + m.getOrElse(i.length, 0)))
}
val w=List("Hallo","das","sind","ein","paar", "Wörter")
countLetters(w)

//c
def countLettersS(l: List[String]): Map[Int, List[String]] = {
  l.groupBy(_.length)
}
countLettersS(w)

def countLettersSS(l: List[String]): Map[Int, List[String]] = {
  l.foldLeft(Map[Int, List[String]]())((m, i) => m.updated(i.length, i +:m.getOrElse(i.length, List())))
}
countLettersSS(w)

//d
def avgNumbersMine(l:List[Int]):Map[Boolean, Double] ={
  val m = l.foldLeft((0, 0, 0, 0))((m, e) => {
    if (e % 2 == 0) (m._1 + e, m._2 + 1, m._3, m._4)
    else (m._1, m._2, m._3 + e, m._4 + 1)
  })
  Map(true-> m._1.toDouble / m._2, false ->m._3.toDouble / m._4)
}
val l2= List(1,4,5,7,8,9)
avgNumbersMine(l)

def avgNumbersHis(l:List[Int]):Map[Boolean, Double]= {
  l.groupBy(_ % 2 == 0).view.
    mapValues(x => x.sum.toDouble / x.size).toMap
}
avgNumbersMine(l)