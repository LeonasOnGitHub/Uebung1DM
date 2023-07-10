val notenliste: List[(Int,(String,String,List[(String,Double)]))]= List((1,("Donald Duck", "WS22",List(("Mathe I", 2.3), ("Prog I",2.0), ("Formale Sprachen",1.7)))),
  (2,("Donald Duck", "SS23",List(("Datenbanken", 2.3), ("Prog II",1.0), ("Software Engineering",1.0)))), (3,("Dagobert Duck", "WS22",List(("Mathe I", 1.3), ("Prog I",2.3), ("Formale Sprachen",2.7)))),
  (4,("Dagobert Duck", "SS23",List(("Datenbanken", 2.0), ("Prog II",2.0), ("Software Engineering",1.7)))), (5,("Tick", "WS22",List(("Mathe I", 3.7), ("Formale Sprachen",4.0)))),
  (6,("Tick", "SS23",List(("Prog I",3.3), ("Datenbanken", 1.0), ("Prog II",2.0), ("Software Engineering",1.0),("Prog III",3.3)))))

def findNumberGradesAboveAverageGrade2(l: List[(Int, (String, String, List[(String, Double)]))]): Map[String, Int] = {
  val gradesPerStudent = l.flatMap(_._2._3) // Flattening the list of grades per student
  val averageGrade = gradesPerStudent.map(_._2).sum / gradesPerStudent.length // Calculating the average grade

  l.groupBy(_._2._1) // Grouping the list by student name
    .mapValues(_.flatMap(_._2._3)) // Flattening the grades for each student
    .mapValues(_.count(_._2 > averageGrade))
    .toMap// Counting the grades above average for each student
}
findNumberGradesAboveAverageGrade2(notenliste)

def findNumberGradesAboveAverageGrade(l: List[(Int, (String, String, List[(String, Double)]))]): Map[String, Int] = {
  val gradesPerStudent = l.flatMap(_._2._3)
  val averageGrade = gradesPerStudent.map(_._2).sum / gradesPerStudent.length

  l.groupBy(_._2._1)
    .mapValues(_.flatMap(_._2._3))
    .mapValues(_.count(_._2 > averageGrade))
    .toMap
}
findNumberGradesAboveAverageGrade(notenliste)