val l1 = List(1, 2, 3, 4)

val m = l1.foldLeft((0, 0, 0, 0))((m, e) => {
  if (e % 2 == 0) (m._1 + e, m._2 + 1, m._3, m._4)
  else (m._1, m._2, m._3 + e, m._4 + 1)
})
val avg = (m._1.toDouble / m._2, m._3.toDouble / m._4)

l1.foldLeft(List.empty[Int])((a, b) => {
  b :: a
})

l1.map { case (word) => (word, word)}
l1.flatMap(x=> List(x,x))


