val s1="This 88 is! a,Test! The result !!!should be: 9 Words"
val words_s1= List("a", "be", "is", "result", "should", "test", "the", "this", "words")
val s2="This is another test. It contains a lot of words which are also in string 1."
val words_s2= List("a", "also", "another", "are", "contains", "in", "is", "it", "lot", "of", "string", "test", "this", "which", "words")
val test_list= List((0,s1),(1,""),(2,s2),(3,""))
val wordlist=List("a", "a", "also", "another", "are", "be", "contains", "in", "is", "is", "it", "lot", "of", "result", "should", "string",
  "test", "test", "the", "this", "this", "which", "words", "words")
val wordOccurences= List(("a",2), ("also",1), ("another",1), ("are",1), ("be",1), ("contains",1), ("in",1), ("is",2),
  ("it",1), ("lot",1), ("of",1), ("result",1), ("should",1), ("string",1), ("test",2), ("the",1), ("this",2), ("which",1), ("words",2))
val wordsWithIndex= List((0,"a"), (0,"be"), (0,"is"), (0,"result"), (0,"should"), (0,"test"), (0,"the"), (0,"this"), (0,"words"), (2,"a"),
  (2,"also"), (2,"another"), (2,"are"), (2,"contains"), (2,"in"), (2,"is"), (2,"it"), (2,"lot"), (2,"of"), (2,"string"), (2,"test"),
  (2,"this"), (2,"which"), (2,"words"))
val inverseIndex= Map("test" -> List(0, 2), "this" -> List(0, 2), "in" -> List(2), "are" -> List(2), "is" -> List(0, 2), "another" -> List(2),
  "result" -> List(0), "it" -> List(2), "a" -> List(0, 2), "string" -> List(2), "also" -> List(2), "should" -> List(0), "lot" -> List(2),
  "words" -> List(0, 2), "which" -> List(2), "be" -> List(0), "contains" -> List(2), "of" -> List(2), "the" -> List(0))


def getWords(line:String):List[String]={
  /*
   * Extracts all words from a line
   *
   * 1. Removes all characters which are not letters (A-Z or a-z)
   * 2. Shifts all words to lower case
   * 3. Extracts all words and put them into a list of strings
   */
  line.replaceAll("\\d+", "").toLowerCase().split("\\W+").toList
}

def getAllWords(l:List[(Int,String)]):List[String]={
  /*
   * Extracts all words from a List containing line number and line tuples
   * The words should be in the same order as they occur in the source document
   *
   * Hint: Use the flatMap function
   */
  l.flatMap(x => getWords(x._2)).filter(s => s.nonEmpty)
}

def countWords(l:List[String]):List[(String,Int)]={
  /*
   *  Gets a list of words and counts the occurrences of the individual words
   */
  l.groupBy(identity)
    .map { case (word, occurrences) => (word, occurrences.length) }
    .toList
}
countWords(getAllWords(test_list))

val list = List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
val groupedMap = list.groupBy(identity)

println(groupedMap)

def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
  l.flatMap(x => getWords(x._2).filter(s => s.nonEmpty).map(i => (x._1, i)))
}

getAllWordsWithIndex(test_list).foldLeft(Map[String, List[Int]]())((a,b) => a.updated(b._2, b._1 :: a.getOrElse(b._2, List())))

inverseIndex.get("test")
