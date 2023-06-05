//Vorlesung Aufgabe flatten
def flatten(l: List[Any]): List[Any] = l match {
  //Lehre Liste /ende der Liste
  case Nil => Nil
  // der Kopf der Liste ist eine Liste, ++ verknüpft zwei listen
  case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
  // :: verknüft ein Element mit einer Liste
  case head :: tail => head :: flatten(tail)
}

// Vorlesung Application of the for loop
val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus",
  "jamesBond"), ("francesco", "die12KammernDerShaolin"))

//which films have been seen by „francesco“
for (x <- db if (x._1 == "francesco")) yield x._2
db.filter(X => X._1 == "francesco").map(_._2)

//who have seen the film „jamesBond“
for (x <- db if (x._2 == "jamesBond")) yield x._1
db.filter(X => X._2 == "jamesBond").map(_._1)

//who have seen more than one film
for (f1 <- db; f2 <- db if (f1._1 == f2._1 && f1._2 != f2._2)) yield f1._1
db.flatMap(x => db.map(y => (x, y))).filter(z => z._1._1 == z._2._1 && z._1._2 != z._2._2).map(_._1._1)

//who have seen what films
for (f1 <- db; f2 <- db if(f1._2 == f2._2 && f1._1 != f2._1)) yield f1._1
