import scala.io.Source
import scala.io.StdIn

case class Score(green: Int, yellow: Int, red: Int) {
  def this(score: String) = {
    this(score(0).toInt - '0',
         score(1).toInt - '0',
         score(2).toInt - '0')
  }

  override def toString(): String = s"$green$yellow$red"
}

def getScore(hidden: String, guess: String): Score = {
  val (hiddenNoGreen, guessNoGreen) = hidden.zip(guess)
                                            .filter(p => p._1 != p._2)
                                            .unzip
  val green = guess.length - guessNoGreen.length
  val yellow = guessNoGreen.intersect(hiddenNoGreen).length
  val red = guess.length - green - yellow
  Score(green, yellow, red)
}

def evaluateCandidate(candidate: String, possibilities: List[String]): Map[Score, List[String]] = {
  possibilities.groupBy(getScore(_, candidate))
}

def log2(x: Double): Double = {
  import scala.math.log10
  log10(x) / log10(2)
}

def entropy(choice: Map[Score, List[String]]): Double = {
  val total = choice.values.map(_.length).sum
  choice.values.map(_.length.toDouble / total)
               .map(x => x * log2(x))
               .sum
}

def choose(candidates: List[String]): String = {
  candidates.maxBy(c => entropy(evaluateCandidate(c, candidates)))
}

@main def word500() = {
  val lines = Source.fromFile("words").getLines().toList
  var words = lines.map(_.split(" ")(0))
  while (words.length > 1) {
    System.out.print("guess: ")
    val guess = StdIn.readLine()
    System.out.print("score: ")
    val score = new Score(StdIn.readLine())
    System.out.println(guess + " " + score)
    words = words.filter(word => {
      score == getScore(word, guess) 
    })
    System.out.println(words.length)
    words.foreach(println(_))
    // val bestChoice = choose(words)
    // System.out.println("Best guess: "+bestChoice)
  } 
}
