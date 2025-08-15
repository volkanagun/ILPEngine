//noinspection SourceNotClosed
package ilp.experiments

import ilp.data.database.{Bias, Database}
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Parser}
import ilp.data.variables.Variable

import java.io.File
import java.util.regex.Pattern
import scala.io.Source

class Experiment(val params: Params):

  val name = params.experimentName
  var folder = "examples/" + name + "/"
  var database = new Database(name)

  var hypothesis: Hypothesis = null
  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()

  def getParams = params
  def getDatabase = database

  def getHypothesis = hypothesis

  def getPositives = positives

  def getNegatives = negatives

  def load(): this.type =
    loadDatabase()
    loadSamples()
    loadQueries()


  def pruneDatabase():this.type =
    database = database.prune(getRelevant(), getIrrelevant())
    this

  def getRelevant():Set[Variable] =
    positives.flatMap(predicate=> predicate.getVariables)

  def getIrrelevant():Set[Variable] =
    negatives.flatMap(predicate=> predicate.getVariables)

  private def loadSamples(): this.type =
    println("Loading samples")
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
          if negative then negatives += predicate.toNegative else positives += predicate
      })

    println("Loading samples finished.")
    this

  def loadDatabase(): this.type =
    println(s"Loading database from ${folder}")
    Source.fromFile(folder + "bk.pl").getLines().map(_.trim)
      .filter(line=> line.nonEmpty && !line.startsWith("%") && !line.contains(":-"))
      .foreach(line => {
        val predicate = Parser.parsePredicate(line).get
        database.add(predicate)
      })
    println("Loading database finished.")
    val bias = Bias().build(folder + "bias.pl")
    database.build().setBias(bias)
    this

  def loadQueries(): this.type =
    println("Loading queries: " + folder)
    val fname = folder + "/query.pl"
    if File(fname).exists() then
      val rules = Source.fromFile(folder + "/query.pl").getLines().map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("%"))
        .map(line => {
          val rule = Parser.parseRule(line).get
          rule
        }).toArray
      hypothesis = Hypothesis(rules).build()
    println("Loading queries finished.")
    this

