val immos: List[(Int, (String, Int, Int, Int, List[String]))] = List((1, ("Schöneweide", 60, 3, 500,
  List("Badewanne", "Einbauküche", "Fahrstuhl"))), (2, ("Schöneweide", 50, 2, 400,
  List("Badewanne", "Einbauküche"))), (3, ("Schöneweide", 25, 1, 400, List("Einbauküche",
  "Fahrstuhl"))), (4, ("Schöneweide", 20, 1, 350, List("Kabelfernsehen", "Internet"))), (5,
  ("Prenzelberg", 60, 3, 1200, List("Internet"))), (6,
  ("Prenzelberg", 100, 3, 1800, List("Badewanne", "Einbauküche", "Fahrstuhl"))), (7, ("Steglitz",
  100, 4, 1200, List("Badewanne", "Einbauküche", "Fahrstuhl", "Kabelfernsehen", "Internet"))),
  (8, ("Steglitz", 100, 4, 1200, List("Badewanne", "Einbauküche", "Internet"))), (9, ("Steglitz",
    120, 4, 1200, List("Badewanne", "Einbauküche", "Internet"))))

def mapper[KeyIn, ValueIn, KeyMOut, ValueMOut](mapFun:
                                               ((KeyIn, ValueIn)) => List[(KeyMOut, ValueMOut)],
                                               data: List[(KeyIn, ValueIn)]): List[(KeyMOut, ValueMOut)] = {
  data.flatMap(mapFun(_))
}
def sorter[KeyMOut, ValueMOut](data: List[(KeyMOut, ValueMOut)]): List[(KeyMOut, List[ValueMOut])] = {
  data.groupBy(_._1).mapValues(X => X.map(_._2)).toList
}
def reducer[KeyMOut, ValueMOut, KeyROut, ValueROut](redFun:
                                                    ((KeyMOut, List[ValueMOut])) => List[(KeyROut, ValueROut)],
                                                    data: List[(KeyMOut, List[ValueMOut])]): List[(KeyROut, ValueROut)] = {
  data flatMap (redFun(_))
}
def mapReduce[KeyIn, ValueIn, KeyMOut, ValueMOut, KeyROut, ValueROut](mapFun:
                                                                      ((KeyIn, ValueIn)) => List[(KeyMOut, ValueMOut)],
                                                                      redFun:
                                                                      (((KeyMOut, List[ValueMOut])) => List[(KeyROut, ValueROut)]), data: List[(KeyIn, ValueIn)]):
List[(KeyROut, ValueROut)] = {
  reducer(redFun, sorter(mapper(mapFun, data)))
}
def findeAnzahlAusstattungsmerkmale(data: List[(Int, (String, Int, Int, Int, List[String]))]): List[(String, Int)] = {
  data.flatMap(x => x._2._5).foldLeft(Map[String, Int]())((m, e) => m.updated(e, 1 + m.getOrElse(e, 0))).toList
}
def findeAnzahlAusstattungsmerkmale2(data: List[(Int, (String, Int, Int, Int, List[String]))]): List[(String, Int)] = {
  mapReduce[Int, (String, Int, Int, Int, List[String]), String, Int, String, Int](
    x => x._2._5.map((_,1)),
    x=> List((x._1, x._2.size)),
    data
  )
}
findeAnzahlAusstattungsmerkmale(immos)
findeAnzahlAusstattungsmerkmale2(immos)

val immos: List[(Int, (String, Int, Int, Int, List[String]))] = List((1, ("Schöneweide", 60, 3, 500,
  List("Badewanne", "Einbauküche", "Fahrstuhl"))), (2, ("Schöneweide", 50, 2, 400,
  List("Badewanne", "Einbauküche"))), (3, ("Schöneweide", 25, 1, 400, List("Einbauküche",
  "Fahrstuhl"))), (4, ("Schöneweide", 20, 1, 350, List("Kabelfernsehen", "Internet"))), (5,
  ("Prenzelberg", 60, 3, 1200, List("Internet"))), (6,
  ("Prenzelberg", 100, 3, 1800, List("Badewanne", "Einbauküche", "Fahrstuhl"))), (7, ("Steglitz",
  100, 4, 1200, List("Badewanne", "Einbauküche", "Fahrstuhl", "Kabelfernsehen", "Internet"))),
  (8, ("Steglitz", 100, 4, 1200, List("Badewanne", "Einbauküche", "Internet"))), (9, ("Steglitz",
    120, 4, 1200, List("Badewanne", "Einbauküche", "Internet"))))

def mapper[KeyIn, ValueIn, KeyMOut, ValueMOut](mapFun:
                                               ((KeyIn, ValueIn)) => List[(KeyMOut, ValueMOut)],
                                               data: List[(KeyIn, ValueIn)]): List[(KeyMOut, ValueMOut)] = {
  data.flatMap(mapFun(_))
}
def sorter[KeyMOut, ValueMOut](data: List[(KeyMOut, ValueMOut)]): List[(KeyMOut, List[ValueMOut])] = {
  data.groupBy(_._1).mapValues(X => X.map(_._2)).toList
}
def reducer[KeyMOut, ValueMOut, KeyROut, ValueROut](redFun:
                                                    ((KeyMOut, List[ValueMOut])) => List[(KeyROut, ValueROut)],
                                                    data: List[(KeyMOut, List[ValueMOut])]): List[(KeyROut, ValueROut)] = {
  data flatMap (redFun(_))
}
def mapReduce[KeyIn, ValueIn, KeyMOut, ValueMOut, KeyROut, ValueROut](mapFun:
                                                                      ((KeyIn, ValueIn)) => List[(KeyMOut, ValueMOut)],
                                                                      redFun:
                                                                      (((KeyMOut, List[ValueMOut])) => List[(KeyROut, ValueROut)]), data: List[(KeyIn, ValueIn)]):
List[(KeyROut, ValueROut)] = {
  reducer(redFun, sorter(mapper(mapFun, data)))
}

def immosMitBadewanneProBezirk(data: List[(Int, (String, Int, Int, Int, List[String]))]):
                                                                        List[(String, List[(Int, Int)])] = {
  mapReduce[Int,(String,Int,Int,Int,List[String]),String, (Int,Int), String, List[ (Int,Int)] ](
    x=> if (x._2._5.contains("Badewanne")) List((x._2._1,(x._1,x._2._2))) else List(),
    x=> List((x._1,x._2)),
    data
  )
}