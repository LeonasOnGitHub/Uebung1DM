import scala.annotation.tailrec
import scala.collection.immutable.List

//Aufgabe 1
abstract class IntList {
  def isEmpty: Boolean

  def head: Int

  def tail: IntList

  def prefix(elem: IntList): IntList

  def flip: IntList

  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList
}

case object Empty extends IntList {
  def isEmpty = true

  def head = throw new Error("List is Empty")

  def tail = throw new Error("List is Empty")

  def prefix(elem: IntList): IntList = Empty

  def flip: IntList = this

  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList = this
}

case class Cons(head: Int, tail: IntList) extends IntList {
  def isEmpty = false

  def prefix(elem: IntList): IntList = elem match {
    case Empty => this
    case Cons(h, t) => Cons(h, prefix(t))
  }

  def flip: IntList =
    Cons(head, Empty).prefix(tail.flip)

  def flip3: IntList = this match {
    case Cons(_, Empty) => this
    case Cons(head, tail) => Cons(head, Empty).prefix(tail.flip)
  }

  def flip2: IntList = {
    def flipHelper(list: IntList, acc: IntList): IntList = list match {
      case Cons(head, tail) => flipHelper(tail, Cons(head, acc))
      case Empty => acc
    }

    flipHelper(this, Empty)
  }


  def changeNumber(pred: Int => Boolean, change: Int => IntList): IntList = pred(head) match {
    case true => tail.changeNumber(pred, change).prefix(change(head))
    case false => Cons(head, tail.changeNumber(pred, change))
  }
}

Cons(1, Cons(2, Cons(3, Empty))).flip2
Cons(1, Cons(2, Cons(3, Empty))).flip

Cons(1, Cons(2, Cons(3, Empty))).changeNumber(_ % 2 == 0, x => Cons(x, Cons(x, Empty)))

// Aufgabe 2
val calories: List[(String, String, List[(String, Int)])] =
  List(("Donald Duck", "2022-01-01", List(("Frühstück", 800), ("Mittag",
    700), ("Snack", 200), ("Abendbrot", 500))), ("Donald Duck", "2022-01-02",
    List(("Frühstück", 700), ("Mittag", 650), ("Abendbrot", 520))),
    ("Donald Duck", "2022-01-03", List(("Frühstück", 800), ("Mittag", 700),
      ("Snack", 200), ("Abendbrot", 500), ("Snack", 150))), ("Donald Duck",
      "2022-01-04", List(("Frühstück", 850), ("Mittag", 900), ("Snack", 500),
      ("Snack", 400))), ("Donald Duck", "2022-01-05", List(("Frühstück", 600),
      ("Mittag", 700), ("Snack", 200), ("Abendbrot", 100))), ("Dagobert Duck",
      "2022-01-01", List(("Frühstück", 300), ("Mittag", 500), ("Snack", 100),
      ("Abendbrot", 200))), ("Dagobert Duck", "2022-01-02",
      List(("Frühstück", 200), ("Mittag", 300), ("Snack", 400), ("Abendbrot",
        200))), ("Dagobert Duck", "2022-01-03", List(("Frühstück", 800),
      ("Mittag", 700), ("Snack", 200), ("Snack", 200))), ("Dagobert Duck",
      "2022-01-04", List(("Frühstück", 200), ("Mittag", 300), ("Snack", 200),
      ("Snack", 500))), ("Dagobert Duck", "2022-01-05", List(("Frühstück", 200),
      ("Mittag", 700), ("Abendbrot", 500))))

def dayWithMaxCalories(l: List[(String, String, List[(String, Int)])]): (String, String, Int) = {
  l.foldLeft(("", "", 0))((x, s) => {
    val cal = s._3.foldLeft(0: Int)((a, b) => a + b._2)
    if (x._3 <= cal) {
      (s._1, s._2, cal)
    }
    else x
  })
}

dayWithMaxCalories(calories)

def dayWithMaxCalories2(l: List[(String, String, List[(String, Int)])]): (String, String, Int) = {
  l.map {
    case (n, t, c) => (n, t, c.foldLeft(0)((a, b) => a + b._2))
  }.maxBy { case (_, _, cals) => cals }
}
dayWithMaxCalories2(calories)


//b
def caloriesByMeal(l: List[(String, String, List[(String, Int)])]): Map[String, Int] = {
  l.flatMap {
    case (_, _, mc) => (mc)
  }.foldLeft(Map[String, Int]())((m, ll) => m.updated(ll._1, ll._2 + m.getOrElse(ll._1, 0)))

}
caloriesByMeal(calories)

def caloriesByMeal2(l: List[(String, String, List[(String,
  Int)])]): Map[String, Int] = {
  l.flatMap(x => x._3).
    foldLeft(Map(): Map[String, Int])((map, el) => map.updated(el._1,
      map.getOrElse(el._1, 0) + el._2))
}
caloriesByMeal2(calories)

def caloriesByMeal3(l: List[(String, String, List[(String, Int)])]): Map[String, Int] = {
  l.foldLeft(Map.empty[String, Int]) { case (acc, (_, _, meals)) =>
    meals.foldLeft(acc) { case (mealMap, (meal, calories)) =>
      mealMap.updated(meal, mealMap.getOrElse(meal, 0) + calories)
    }
  }
}
caloriesByMeal3(calories)

//c
def  caloriesByPerson(l:List[(String, String, List[(String, Int)])]):Map[String,Int] = {
  l.map {
    case (n, _, mc) => (n, mc.foldLeft(0)((a, b) => a + b._2))
  }.foldLeft(Map[String, Int]())((m, ll) => m.updated(ll._1, ll._2 + m.getOrElse(ll._1, 0)))
}
caloriesByPerson(calories)

def caloriesByPerson2(l: List[(String, String, List[(String,
  Int)])]):Map[String,Int]={
  l.flatMap(e=>e._3.map(x=>(e._1,x._2))).
    foldLeft(Map():Map[String,Int])((map,e)=>map.updated(e._1,map.getOrElse(e._1,0)+e._2))
}
caloriesByPerson2(calories)

//Aufgabe 3
//Map[String,List[Int]]
val m= Map(1->List(2,3,4,5), 2->List(1,2,3), 3->List(4,5))
m.map{
  case (k,v)=> (k, v.sum.toDouble/v.length)
}