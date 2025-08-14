package ilp.experiments

import ilp.data.program.{Parser, Rule}


class Params(var experimentName: String = "zendo2") extends Serializable:

  var windowSize: Int = 4
  var resembleWindow: Int = 3
  var iterationsSize = 10
  var recursionSize = 5
  var filterSize = 100
  var maxRules = 30

  var binaryPositiveThreshold: Double = 0.0
  var binaryNegativeThreshold: Double = 0.7
  var unionPositiveThreshold: Double = 0.01
  var unionNegativeThreshold: Double = 0.0
  var resembleThreshold: Double = 0.9
  var scoreThreshold: Double = 0.9

  var experiments = Set("kinship-ancestor", "kinship-pi", "imdb3")
  var engineType = "MIL"

  def toLine(time: Double, score: Double): String =
    s"$experimentName, $scoreThreshold, $windowSize, $iterationsSize, $recursionSize, $filterSize, $time, $score"

  def toCSVHeaderLine(): String =
    "Dataset, Score Threshold, History Window Size, Iteration Size, Recursion Size, Filter Size, Time, Score";


  def generateParams(): Array[Params] =
    Iterator.iterate(0.7)(_ + 0.1)
      .takeWhile(_ <= 1.0).flatMap(crrScoreThreshold => {
        Range(5, 1, -1).flatMap(crrWindowSize => {
          Range(7, 1, -1).flatMap(crrIterationSize => {
            Range(2, 11).flatMap(crrRecursionSize => {
              Range(10, 2100, 100).map(crrFilterSize => {
                val params = Params(experimentName)
                params.scoreThreshold = crrScoreThreshold
                params.windowSize = crrWindowSize
                params.iterationsSize = crrIterationSize
                params.recursionSize = crrRecursionSize
                params.filterSize = crrFilterSize
                params.binaryPositiveThreshold = binaryPositiveThreshold
                params.binaryNegativeThreshold = binaryNegativeThreshold
                params.resembleWindow = resembleWindow
                params.resembleThreshold = resembleThreshold

                params
              })
            })
          })
        })
      }).toArray


  def getRule(str: String): Rule =
    Parser.parseRule(str).get

  def getMeta: Set[Rule] =
    Set(
      getRule("p(X,Y) :- f(X) & p(X,Y).") //,
      /* getRule("p(X,Y) :- f(X) & m(Y)."),
       getRule("p(X,Y) :- f(Z,X) & m(Z,Y)."),
       getRule("p(X,Y) :- f(X,Y) & m(Y)."),
       getRule("p(X,Y) :- f(X,Y) & m(X).")*/)