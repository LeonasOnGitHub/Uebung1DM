import scala.collection.immutable.List

abstract class IntList{
  def isEmpty:Boolean
  def head:Int
  def tail:IntList
  def prefix(elem:IntList):IntList
  def flip:IntList
  def changeNumber(pred:Int=>Boolean, change: Int=>IntList):IntList
}
case object Empty extends IntList{
  def isEmpty = true
  def head= throw new Error ("List is Empty")
  def tail= throw new Error ("List is Empty")
  def prefix(elem:IntList):IntList= Empty
  def flip:IntList= this
  def changeNumber(pred:Int=>Boolean, change: Int=>IntList):IntList= ???
}
case class Cons(head:Int, tail:IntList) extends IntList{
  def isEmpty= false
  def prefix(elem:IntList):IntList= elem match {
    case Empty => this
    case Cons(h,t) => Cons(h, prefix(t))
  }
  def flip:IntList = this match {
    case Cons(_, Empty) => Empty
    case Cons(head, tail) => Cons(head, tail.flip)
    }

  def changeNumber(pred:Int=>Boolean, change: Int=>IntList):IntList= ???
}
Cons(1, List[IntList](2, 3, 4, 5)).flip