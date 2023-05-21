package Uebung3

object TempConv {
  def convert(change:(String, Double)):(String, Double)={
    change match {
      case ("Fahreneit", temp) => ("Fahreneit",temp*1.8+32)
      case ("Reamur",temp) => ("Reamur",temp*0.8)
      case ("Kelvin", temp) => ("Kelvin",temp+273.15)
    }

  }

}
