package ilp.experiments


import ilp.data.{Hypothesis, Parser}
import ilp.data.database.{Database, Engine, Optimized, Plan}

import java.io.{File, PrintWriter}
import scala.io.Source

object Queries {
  val folder = "examples/"
  val resultFilename = "resources/experiments/performance.txt"

  def measureMultipleTime[T](block: => T, count: Int = 5): Double = {
    val time = Range(0, count).map(i => {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      val elapsedTime = (end - start) / 1e6
      elapsedTime
    }).min

    time
  }

  def loadDatabase(filename:String):Database =
    println("Parsing database: " + filename)
    val database = Database(filename, 128)
    Source.fromFile(filename + "/bk.pl").getLines().map(_.trim)
      .filter(_.nonEmpty)
      .foreach(line => {

        if line.startsWith("%") then
          val skip = 1
        else if line.contains(":") then
          val rule = Parser.parseRule(line).get
          database.add(rule)
        else
          val predicate = Parser.parsePredicate(line).get
          database.add(predicate)
      })

    database.build()


  def loadQueries(filename:String):Hypothesis =
    println("Loading queries: "+filename)
    val rules = Source.fromFile(filename + "/query.pl").getLines().map(_.trim)
      .filter(_.nonEmpty)
      .map(line => {
        val rule = Parser.parseRule(line).get
        rule
      }).toSet

    Hypothesis(rules)

  def load():Array[(Database, Hypothesis, String)]= {
    val files = File(folder).listFiles().filter(file=> file.isDirectory).filter(file=>{
      val subList = file.list()
      subList.contains("query.pl")
    })
    val names = files.map(_.getName)
    val databases = files.map(file => loadDatabase(file.getPath))
    val queries = files.map(file=> loadQueries(file.getPath))
    databases.zip(queries).zip(names).map(tuple=> (tuple._1._1, tuple._1._2, tuple._2))
  }

  def experiment(database: Database, hypothesis: Hypothesis, name:String):String=
    var text = ""
    val engine = Engine(database)
    val plan = Plan(database)
    val optimizedNone = hypothesis.getSorted().map(rule=> plan.optimizeNone(rule))
    var crrTime = measureMultipleTime({
      engine.joinIDDData(optimizedNone)
    }, 5)

    text = text + name + "\n"
    text = text + "No index, no optimization : " + crrTime.toString + "\n"

    val optimizedRel = hypothesis.getSorted().map(rule => plan.optimizeRelative(rule))
    crrTime = measureMultipleTime({
      engine.joinIDDData(optimizedRel)
    }, 5)

    text = text + "No index, relative optimization : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinIDDParallel(optimizedNone)
    }, 5)

    text = text + "No index, parallel, no optimization : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinIDDRoaring(optimizedNone)
    }, 5)

    text = text + "Index, parallel, no optimization : " + crrTime.toString + "\n"


    crrTime = measureMultipleTime({
      engine.joinIDDData(optimizedRel)
    }, 5)

    text = text + "No Index, relative optimization : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinIDDParallel(optimizedRel)
    }, 5)

    text = text + "No Index, parallel, relative optimization : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinIDDRoaring(optimizedRel)
    }, 5)

    text = text + "Index, parallel, relative optimization : " + crrTime.toString + "\n"

    text


  def experiment(): Unit = {
    val exp = load()
    val pw = PrintWriter(resultFilename)
    exp.foreach{case(db, query, name)=>{
      println("Experimenting : " + name)
      pw.println(experiment(db, query, name))
      pw.println()
    }}
    pw.close()
  }

  def main(args: Array[String]): Unit = {
    experiment()
  }
}
