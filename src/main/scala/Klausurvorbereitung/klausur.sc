
def mapper[KeyIn ,ValueIn, KeyMOut, ValueMOut](mapFun:((KeyIn,ValueIn))=>List[(KeyMOut,ValueMOut)],
                                               data:List[(KeyIn, ValueIn)]):List[(KeyMOut,ValueMOut)]={
  data.flatMap(mapFun(_))}

def sorter[KeyMOut,ValueMOut](data:List[(KeyMOut,ValueMOut)]):List[(KeyMOut,List[ValueMOut])]= {
  data.groupBy(_._1).mapValues(X=> X.map(_._2)).toList}

def reducer[KeyMOut,ValueMOut,KeyROut, ValueROut](redFun:((KeyMOut,List[ValueMOut]))=>List[(KeyROut,ValueROut)],
                                                  data:List[(KeyMOut,List[ValueMOut])]):List[(KeyROut, ValueROut)]={

  data flatMap (redFun(_))}


def mapReduce[KeyIn,ValueIn,KeyMOut,ValueMOut, KeyROut, ValueROut](mapFun:((KeyIn,ValueIn))=>List[(KeyMOut, ValueMOut)],
                                                                   redFun:(((KeyMOut,List[ValueMOut]))=>List[(KeyROut,ValueROut)]), data:List[(KeyIn,ValueIn)]): List[(KeyROut,ValueROut)] = {

  reducer(redFun,sorter(mapper(mapFun, data)))
}

import scala.util.matching.Regex
val regex: Regex = "^Prog.*".r

val notenliste: List[(Int,(String,String,List[(String,Double)]))]=
  List(
    (1,("Donald Duck", "WS22",List(("Mathe I", 2.3), ("Prog I",2.0), ("Formale Sprachen",1.7)))),
    (2,("Donald Duck", "SS23",List(("Datenbanken", 2.3), ("Prog II",1.0), ("Software Engineering",1.0)))),
    (3,("Dagobert Duck", "WS22",List(("Mathe I", 1.3), ("Prog I",2.3), ("Formale Sprachen",2.7)))),
    (4,("Dagobert Duck", "SS23",List(("Datenbanken", 2.0), ("Prog II",2.0), ("Software Engineering",1.7)))),
    (5,("Tick", "WS22",List(("Mathe I", 3.7), ("Formale Sprachen",4.0)))),
    (6,("Tick", "SS23",List(("Prog I",3.3), ("Datenbanken", 1.0), ("Prog II",2.0), ("Software Engineering",1.0),("Prog III",3.3)))))

def findAverageProgGradePerPerson(l:List[(Int,(String,String,List[(String,Double)]))]): List[(String, Double)] = {
  mapReduce[Int, (String,String,List[(String,Double)]), String, Double, String, Double](
    x => x._2._3.filter(x => regex.matches(x._1)).map(z => (x._2._1, z._2)),
    x => List((x._1, x._2.sum/ x._2.size)),
    l
  )
}

println(findAverageProgGradePerPerson(notenliste))