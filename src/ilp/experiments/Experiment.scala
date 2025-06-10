package ilp.experiments

import ilp.data.database.{Bias, Database}
import ilp.data.predicates.Predicate
import ilp.data.{Hypothesis, Parser}

import java.util.regex.Pattern
import scala.io.Source

class Experiment(params: Params):

  val name = params.experimentName
  var folder = "examples/" + name + "/"
  var database = new Database(name)

  var hypothesis: Hypothesis = null
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()

  def getDatabase() = database

  def getHypothesis() = hypothesis

  def getPositives() = positives

  def getNegatives() = negatives

  protected def loadSamples(): this.type =
    val rSamples = "((pos|neg)\\((.*?)\\)\\.)"
    val pSamples = Pattern.compile(rSamples)
    Source.fromFile(folder + "exs.pl").getLines().map(_.trim)
      .filter(line => !line.startsWith("%"))
      .filter(_.nonEmpty).foreach(line => {
        val matching = pSamples.matcher(line)
        val f = matching.find()
        if f then
          val negative = matching.group(2).equals("neg")
          val item = matching.group(3) + "."
          val predicate = Parser.parsePredicate(item).get
          if negative then negatives += predicate.toNegative() else positives += predicate
      })


    this

  def loadDatabase(): this.type =
    println("Loading database")
    Source.fromFile(folder + "bk.pl").getLines().map(_.trim)
      .filter(_.nonEmpty)
      .foreach(line => {
        val predicate = Parser.parsePredicate(line).get
        database.add(predicate)
      })

    val bias = Bias().build(folder + "bias.pl")
    database.build().setBias(bias)
    this


  def loadQueries(): this.type =
    println("Loading queries: " + folder)
    val rules = Source.fromFile(folder + "/query.pl").getLines().map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("%"))
      .map(line => {
        val rule = Parser.parseRule(line).get
        rule
      }).toArray

    hypothesis = Hypothesis(rules)
    this

  def load(): this.type =
    loadDatabase()
    loadSamples()
    loadQueries()

