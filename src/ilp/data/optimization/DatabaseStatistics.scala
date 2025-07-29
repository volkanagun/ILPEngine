package ilp.data.optimization

import ilp.data.database.Database
import ilp.experiments.{Experiment, Params}

import java.io.{File, PrintWriter}

class DatabaseStatistics(val name: String, val database: Database) extends Serializable{

  var numPredicates = 0d
  var numPredicateTypes = 0d
  var numArityDensity = 0d
  var numPredicateDensity = 0d
  var duplicateRation = 0d
  var maxArity = 0d
  var maxDuplicateRatio = 0d
  var minArity = 0d
  var minDuplicateRatio = 0d
  var densityRatio = 0d

  def build(): this.type =
    val arities = database.getTemplates().map(pair => pair._2.size.toDouble / pair._2.head.getArity())
    numPredicates = database.getPredicates().size.toDouble
    numArityDensity = arities.toArray.sum / numPredicates
    maxArity = database.getTemplates().flatMap(_._2.map(_.getArity())).max
    minArity = database.getTemplates().flatMap(_._2.map(_.getArity())).min
    numPredicateTypes = database.getTemplates().size
    numPredicateDensity = numPredicates / numPredicateTypes

    duplicateRation = database.getStatistics().map{ pair=> {
      pair._2.getDuplicateRatio()
    }}.sum / database.getStatistics().size

    maxDuplicateRatio = database.getStatistics().map{ pair=> {
      pair._2.getDuplicateRatio()
    }}.max

    minDuplicateRatio = database.getStatistics().map{ pair=> {
      pair._2.getDuplicateRatio()
    }}.min

    this

  override def toString(): String = {
    name + "," + numPredicates.toString + ", "+ numPredicateTypes + "," + numPredicateDensity.toString + ", "+numArityDensity+ "," +  maxArity + "," + minArity + "," +
      duplicateRation + "," + maxDuplicateRatio + "," + minDuplicateRatio
  }
}


object DatabaseStatistics {

  val files = new File("examples").listFiles()
  val joinExperiments = Array(/*"ptc", "pte", "acetyl", "dunnhumby1", "iggp", "imdb", "kinship", "protein", "random0", "random1", "random2", "noisy", "suranim", "trains1", "trains2", "uwcs", "webkb",*/ "yeast"/*, "zendo"*/)
  val joinFilenames = files.filter(file => joinExperiments.exists(name => file.getName.startsWith(name))).map(_.getName)
  val filename = "resources/experiments/statistics.csv"

  def main(args: Array[String]): Unit = {

    new PrintWriter(filename){
      println("Database,Number of Entries,Number of Predicates,Predicate Density,Variable Predicate Ratio, Max Variable Size, Min Variable Size, Duplicate Ratio,Max Duplicate Ratio, Min Duplicate Ratio")
      joinFilenames.foreach(name=>{
        val db = Experiment(Params(name)).loadDatabase().getDatabase()
        println(DatabaseStatistics(name, db).build().toString())
      })
    }.close()

  }
}