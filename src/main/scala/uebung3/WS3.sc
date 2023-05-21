//Aufgabe 7
abstract class IntList{
  def isEmpty:Boolean
  def head:Integer
  def tail:IntList

}
case object Empty extends IntList{
  def isEmpty = true
  def head= throw new Error ("List is Empty")
  def tail= throw new Error ("List is Empty")
  def prefix(elem: IntList): IntList = elem

}
case class Cons(head:Integer, tail:IntList) extends IntList{
  def isEmpty= false
  def prefix(elem: IntList): IntList = elem match {
    case Empty => this
    case Cons(h,t) => Cons(h, prefix(t))
  }
}

