package ilp.experiments

import ilp.data.Parser
import ilp.data.database.Database
import ilp.data.predicates.Predicate

import java.util.regex.Pattern
import scala.io.Source

class Experiment(params:Params):

  val name = params.experimentName
  var folder = "examples/"+name+"/"
  var database = new Database(name)

  var positives = Set[Predicate]()
  var negatives = Set[Predicate]()

  protected def loadSamples():this.type =
    val rSamples = "((pos|neg)\\((.*?)\\)\\.)"
    val pSamples = Pattern.compile(rSamples)
    Source.fromFile(folder + "exs.pl").getLines().map(_.trim)
      .filter(_.nonEmpty).foreach(line=>{
        val matching = pSamples.matcher (line)
        val f = matching.find()
        if f then
          val negative = matching.group(2).equals("neg")
          val item  = matching.group(3) + "."
          val predicate = Parser.parsePredicate(item).get
          if negative then negatives += predicate.toNegative() else positives += predicate
    })
    

    this

  protected def loadDatabase():this.type =
    Source.fromFile(folder + "bk.pl").getLines().map(_.trim)
      .filter(_.nonEmpty)
      .foreach(line=>{
        val predicate = Parser.parsePredicate(line).get
        database.add(predicate)
    })

    database.build()
    this

  def load(): this.type =
    loadDatabase()
    loadSamples()
  /*
  def induction():this.type =
    val engine = params.getEngine(database)
      .setPositives(positives)
      .setNegatives(negatives)
    println(engine.induction().map(_.toString).mkString("\n"))
    this
  */
