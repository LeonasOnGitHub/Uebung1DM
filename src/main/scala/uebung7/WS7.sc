//Aus der Vorlesung
def primteiler(z:Int):List[Int]={
  def findPrims(counter: Int, value:Int): List[Int]={ (value % counter) match {
    case 0 if value/counter==1 => List(counter)
    case 0 => counter::findPrims(counter,value / counter)
    case _ => findPrims(counter+1, value)
  }
  }
  findPrims(2,z)
}
primteiler(100)