def calculateAverage(inputList: List[(Int, List[String])]): List[(Int, Double, Int)] = {
  val wordMap = Map("word1" -> 1.0, "word2" -> 2.0, "word3" -> 3.0) // Beispiel-Mapping von Wörtern zu Zahlen

  inputList.map { case (number, words) =>
    val filteredWords = words.filter(wordMap.contains)
    val average = filteredWords.map(word => wordMap(word)).sum / filteredWords.length.toDouble
    (number, average, filteredWords.length)
  }
}

// Beispielaufruf
val inputList = List(
  (1, List("word1", "word2", "word3")),
  (2, List("word2", "word3")),
  (3, List("word1", "word3"))
)

val result = calculateAverage(inputList)
result.foreach(println)
