package ilp.experiments

import ilp.data.program.{Parser, Rule}


class Params(var experimentName: String = "zendo2") extends Serializable:

  var windowSize: Int = 4
  var resembleWindow: Int = 3
  var iterationsSize = 10
  var recursionSize = 15
  var filterSize = 100
  var maxRules = 40

  var binaryPositiveThreshold: Double = 0.01
  var binaryNegativeThreshold: Double = 0.7
  var unionPositiveThreshold: Double = 0.01
  var unionNegativeThreshold: Double = 0.0
  var resembleThreshold: Double = 0.9
  var scoreThreshold: Double = 0.9

  var experiments = Set("kinship-ancestor", "kinship-pi", "imdb3")
  var engineType = "MIL"

  def toLine(searchSize:Int, time: Double, score: Double): String =
    s"$experimentName, $scoreThreshold, $windowSize, $iterationsSize, $recursionSize, $filterSize, $searchSize, $time, $score"

  def toCSVHeaderLine(): String =
    "Dataset, Score Threshold, History Window Size, Iteration Size, Recursion Size, Filter Size, Search Size, Time, Score";


  def generateParams(): Array[Params] =
    Array(0.7).flatMap(crrScoreThreshold => {
        Array(2, 3, 5).flatMap(crrWindowSize => {
          Array(2, 3, 7).flatMap(crrIterationSize => {
            Array(20).flatMap(crrRecursionSize => {
              Array(20, 500, 5000).map(crrFilterSize => {
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