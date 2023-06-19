//Aufgabe 7
abstract class IntList {
  def flip(): IntList

  def isEmpty: Boolean

  def head: Integer

  def tail: IntList

}

case object Empty extends IntList {
  def isEmpty = true

  def head = throw new Error("List is Empty")

  def tail = throw new Error("List is Empty")

  def prefix(elem: IntList): IntList = elem

  def flip():IntList = this
}

case class Cons(head: Integer, tail: IntList) extends IntList {
  def isEmpty = false

  def prefix(elem: IntList): IntList = elem match {
    case Empty => this
    case Cons(h, t) => Cons(h, prefix(t))
  }

  def flip():IntList = this match {
    case Cons(head, Empty) => Cons(head, Empty)
    case Cons(head, tail) => Cons(head, Empty).prefix(tail.flip())
  }
}

Cons(1, Cons(2, Cons(3, Empty))).flip()

def distance(l: IntList): Int = {
  def dHelper(abstand: Int, list:IntList): Int = list match {
    case Cons(_, Empty) => abstand
    case Cons(head, tail) =>
      if (Math.abs(head - tail.head) > abstand) dHelper(Math.abs(head - tail.head), tail)
      else dHelper(abstand, tail)
  }

  dHelper(0, l)
}

distance(Cons(1, Cons(3, Cons(10, (Cons(2, Cons(7,Cons(5, Empty))))))))