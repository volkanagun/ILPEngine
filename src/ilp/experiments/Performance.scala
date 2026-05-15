//noinspection SourceNotClosed
package ilp.experiments

import ilp.data.database.{Database, EngineParallel, EngineRoaringParallel, EngineRoaringSerial, EngineSerial}
import ilp.data.optimization.Plan
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Parser, Substitution}
import org.openjdk.jol.info.GraphLayout

import java.io.{File, PrintWriter}
import java.util.regex.Pattern
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.io.Source

object Performance {
  private val folder = "examples/"
  //val joinExperiments = Array(/*"ptc"*//*,"pte","acetyl","dunnhumby1",*//*"iggp"*//*, "imdb", "kinship", "protein", "random0","random1","random2",  "noisy","suranim","trains1", "trains2", "uwcs","webkb","zendo", "yeast"*/)
  val joinExperiments = Array("zendo3")
  //private val joinExperiments = Array("iggp", "uwcs","zendo","webkb","dunnhumby1")
  //val joinExperiments = Array("yeast")
  private val functionalExperiments = Array("robots-functional","robots-linear","synthesis-next")
  private val resultFilename = "resources/experiments/performance.csv"

  def measureMultipleTime[T](block: => T, count: Int = 5): (T, Double) = {

    val time = Range(0, count).map(i => {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      val elapsedTime = (end - start) / 1e6
      (result, elapsedTime)
    }).minBy(_._2)

    time
  }

  def loadDatabase(filename: String): Database =
    println("Parsing database: " + filename)
    val database = Database(filename)
    Source.fromFile(filename + "/bk.pl").getLines().map(_.trim)
      .filter(_.nonEmpty)
      .foreach(line => {

        if line.startsWith("%") then
          val skip = 1
        else if line.contains(":") then
          val rule = Parser.parseHypothesis(line).get
          database.add(rule)
        else
          val predicate = Parser.parsePredicate(line).get
          database.add(predicate)
      })

    database.build()

  protected def loadSubstitutions(folder: String): Set[Predicate] =
    val rSamples = "((pos|neg)\\((.*?)\\)\\.)"
    val pSamples = Pattern.compile(rSamples)
    println(s"Loading samples from : ${folder}/exs.pl")

    Source.fromFile(folder + "/exs.pl").getLines().map(_.trim)
      .filter(_.nonEmpty).flatMap(line => {
        val matching = pSamples.matcher(line)
        val f = matching.find()
        if f then
          val negative = matching.group(2).equals("neg")
          if !negative then
            val item = matching.group(3) + "."
            val predicate = Parser.parsePredicate(item).get
            Some(predicate)
          else
            None
        else
          None
      }).toSet

  protected def loadQueries(filename: String): Hypothesis =
    println("Loading queries: " + filename)
    val rules = Source.fromFile(filename + "/query.pl").getLines().map(_.trim)
      .filter(_.nonEmpty)
      .filter(line => !line.startsWith("%%"))
      .map(line => {
        val rule = Parser.parseRule(line).get
        rule
      }).toArray

    Hypothesis(rules)

  def loadJoin(): Array[(Database, Hypothesis, Set[Predicate], String)] = {
    val files = File(folder).listFiles().filter(file => file.isDirectory)
      .filter(file => joinExperiments.exists(starting => file.getName.startsWith(starting)))
      .filter(file => {
        val subList = file.list()
        subList.contains("query.pl")
      })
    val names = files.map(_.getName)
    val databases = files.map(file => loadDatabase(file.getPath))
    val queries = files.map(file => loadQueries(file.getPath))
    val tests = files.map(file => loadSubstitutions(file.getPath))

    databases.zip(queries).zip(names).zip(tests).map(tuple => (tuple._1._1._1, tuple._1._1._2, tuple._2, tuple._1._2))
  }

  def loadFunctional(): Array[(Database, Hypothesis, Set[Predicate], String)] = {

    val files = File(folder).listFiles().filter(file => file.isDirectory)
      .filter(file => functionalExperiments.exists(starting => file.getName.startsWith(starting)))
      .filter(file => {
        val subList = file.list()
        subList.contains("query.pl")
      })

    val names = files.map(_.getName)
    val databases = files.map(file => loadDatabase(file.getPath))
    val queries = files.map(file => loadQueries(file.getPath))
    val tests = files.map(file => loadSubstitutions(file.getPath))

    databases.zip(queries).zip(names).zip(tests).map(tuple => (tuple._1._1._1, tuple._1._1._2, tuple._2, tuple._1._2))
  }

/*  def initialize(hypothesis: Hypothesis, instances: Set[Predicate], limit: Int = 5): Hypothesis = {
    val allQuery = instances.take(limit).flatMap(predicate => hypothesis.getSorted().map(rule => rule.call(predicate).toRule()))
    Hypothesis(hypothesis.getHead(), allQuery.toArray)
  }*/

/*  def update(database: Database, query: Hypothesis, instances: Set[Predicate]): Unit = {
    val rules = database.getRules()
    val facts = instances.flatMap(instance => query.getSorted().map(rule => rule.call(instance)))
      .flatMap(query => query.getBody()).flatMap(predicate => {
        val opt = rules.find(rule => rule.getHead().identifier() == predicate.identifier())
        if opt.isDefined then {
          val q = opt.get.call(predicate)
          database.facts(q)
        } else
          Set()
      })

    facts.foreach(predicate => database.add(predicate))
    database.build()
  }*/


  def experiment(database: Database, query: Hypothesis, name: String): String =
    var text = ""


    val plan = Plan(database)
    val hypothesis = query

    val (optimizedNone, timeNone) = measureMultipleTime(plan.optimizeNone(hypothesis))
    val (optimizedBellman, timeBellman) = measureMultipleTime(plan.optimizeBellmanFord(hypothesis))
    val (optimizedExperimental, timeIterative) = measureMultipleTime(plan.optimizeExperimental(hypothesis))

    /*println(s"Query optimization (${database.name}) for None: ${timeNone}")
    println(s"Query optimization (${database.name}) for Bellman: ${timeBellman}")
    println(s"Query optimization (${database.name}) for Iterative: ${timeIterative}")*/
    println(s"In memory size for ${database.name}: " + GraphLayout.parseInstance(database).totalSize() + " predicate size: "+database.getPredicates.size)

/*
    val engineSerial = EngineSerial(database)
    val (result1, crrTime1)  = measureMultipleTime({
      engineSerial.join(optimizedNone, Substitution())
    })

    println("No Index, Serial, No Optimization, Count:"+result1.size)
    text = text + s"${name}, No Index, Serial, No Optimization," + crrTime1.toString + "\n"
    val (result2, crrTime2) = measureMultipleTime({
      engineSerial.join(optimizedBellman, Substitution())
    })

     println("No Index, Serial, Bellmanford Optimization, Count:" + result2.size)
    text = text + s"${name}, No Index, Serial, Bellmanford Optimization," + crrTime2.toString + "\n"

    val (result3, crrTime3) = measureMultipleTime({
      engineSerial.join(optimizedExperimental, Substitution())
    })


    println("No Index, Serial, Iterative Optimization, Count:" + result3.size)
    text = text + s"${name}, No Index, Serial, Iterative Optimization," + crrTime3.toString + "\n"
  */





    val engineParallel = EngineParallel(database, 10)
    val (result4, crrTime4) = measureMultipleTime({
      engineParallel.join(optimizedNone, Substitution())
    })

    println("No Index, Parallel, No Optimization, Count:" + result4.size)
    text = text + s"${name}, No Index, Parallel, No Optimization, " + crrTime4.toString + "\n"


    val (result5, crrTime5) = measureMultipleTime({
      engineParallel.join(optimizedBellman, Substitution())
    })

    println("No Index, Parallel, Bellmanford Optimization, Count:" + result5.size)
    text = text + s"${name}, No Index, Parallel, BellmanFord Optimization, " + crrTime5.toString + "\n"

    val (result6, crrTime6) = measureMultipleTime({
      engineParallel.join(optimizedExperimental, Substitution())
    })

    println("No Index, Parallel, Iterative Optimization, Count:" + result6.size)
    text = text + s"${name}, No Index, Parallel, Iterative Optimization, " + crrTime6.toString + "\n"

    val engineRoaring = EngineRoaringSerial(database, 50)

    val (result7, crrTime7) = measureMultipleTime({
      engineRoaring.join(optimizedNone, Substitution())
    })

    println("Roaring Index, Serial, No Optimization, Count:" + result7.size)
    text = text + s"${name}, Roaring Index, Serial, No Optimization, " + crrTime7.toString + "\n"

    val (result8, crrTime8) = measureMultipleTime({
      engineRoaring.join(optimizedBellman, Substitution())
    })

    println("Roaring Index, Serial, Bellmanford Optimization, Count:" + result8.size)
    text = text + s"${name}, Roaring Index, Serial, BellmanFord Optimization, " + crrTime8.toString + "\n"

    val (result9, crrTime9) = measureMultipleTime({
      engineRoaring.join(optimizedExperimental, Substitution())
    })

    println("Roaring Index, Serial, Iterative Optimization, Count:" + result9.size)
    text = text + s"${name}, Roaring Index, Serial, Iterative Optimization, " + crrTime9.toString + "\n"


    val engineRoaringParallel1 = EngineRoaringParallel(database, 10)
    val (result10, crrTime10) = measureMultipleTime({
      engineRoaringParallel1.join(optimizedNone, Substitution())
    })

    println("Roaring Index, Parallel, No Optimization, Count:" + result10.size)
    text = text + s"${name}, Roaring Index, Parallel, No Optimization, " + crrTime10.toString + "\n"
    val engineRoaringParallel2 = EngineRoaringParallel(database, 10)
    val (result11, crrTime11) = measureMultipleTime({
      engineRoaringParallel2.join(optimizedBellman, Substitution())
    }, 5)

    println("Roaring Index, Parallel, Bellmanford Optimization, Count:" + result11.size)
    text = text + s"${name}, Roaring Index, Parallel, BellmanFord Optimization, " + crrTime11.toString + "\n"

    val engineRoaringParallel3 = EngineRoaringParallel(database, 10)
    val (result12, crrTime12) = measureMultipleTime({
      engineRoaringParallel3.join(optimizedExperimental, Substitution())
    }, 5)

    println("Roaring Index, Parallel, Iterative Optimization, Count:" + result12.size)
    text = text + s"${name}, Roaring Index, Parallel, Iterative Optimization, " + crrTime12.toString + "\n"
    println(text)
    text

  def experimentFunctional(database: Database, query: Hypothesis, predicates:Set[Predicate], name: String): String =
    var text = ""


    val plan = Plan(database)
    val hypothesis = query

    val optimizedNone = plan.optimizeNone(hypothesis)
    val optimizedRel = plan.optimizeBellmanFord(hypothesis)
    val optimizedExperimental = plan.optimizeExperimental(hypothesis)
    val hypothesisHead = optimizedNone.last.getHead

    val engineSerial = EngineSerial(database)
    var (result1, crrTime1) = measureMultipleTime({
      predicates.foreach(predicate=>{
        engineSerial.join(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime1 = crrTime1 / predicates.size
    text = text + s"${name}, No Index, Serial, No Optimization," + crrTime1.toString + "\n"


    var (result2, crrTime2) = measureMultipleTime({
      predicates.foreach(predicate => {
        engineSerial.join(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime2 = crrTime2 / predicates.size
    text = text + s"${name}, No Index, Serial, BellmanFord Optimization," + crrTime2.toString + "\n"

    var (result3, crrTime3) = measureMultipleTime({
      predicates.foreach(predicate => {
        engineSerial.join(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime3 = crrTime3 / predicates.size
    text = text + s"${name}, No Index, Serial, Iterative Optimization," + crrTime3.toString + "\n"

    val engineParallel = EngineParallel(database, 10)
    var(result4, crrTime4) = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engineParallel.join(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime4 = crrTime4 / predicates.size
    text = text + s"${name}, No Index, Parallel, No Optimization, " + crrTime4.toString + "\n"

    var (result5, crrTime5) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engineParallel.join(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime5 = crrTime5 / predicates.size
    text = text + s"${name}, No Index, Parallel, BellmanFord Optimization, " + crrTime5.toString + "\n"

    var (result6, crrTime6) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engineParallel.join(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime6 = crrTime6 / predicates.size
    text = text + s"${name}, No Index, Parallel, Iterative Optimization, " + crrTime6.toString + "\n"

    val engineRoaring = EngineRoaringSerial(database, 10)
    var (result7, crrTime7) = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engineRoaring.join(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })
    crrTime7 = crrTime7 / predicates.size
    text = text + s"${name}, Roaring Index, Serial, No Optimization, " + crrTime7.toString + "\n"

    var (result8, crrTime8) = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engineRoaring.join(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })
    crrTime8 = crrTime8 / predicates.size
    text = text + s"${name}, Roaring Index, Serial, BellmanFord Optimization, " + crrTime8.toString + "\n"


    var (result9, crrTime9) = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engineRoaring.join(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })
    crrTime9 = crrTime9 / predicates.size
    text = text + s"${name}, Roaring Index, Serial, Iterative Optimization, " + crrTime9.toString + "\n"


    val engineRoaringParallel = EngineRoaringParallel(database, 10)
    var (result10, crrTime10) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engineRoaringParallel.join(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime10 = crrTime10 / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, No Optimization, " + crrTime10.toString + "\n"

    var (result11, crrTime11) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engineRoaringParallel.join(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime11 = crrTime11 / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, BellmanFord Optimization, " + crrTime11.toString + "\n"

    var (result12, crrTime12) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engineRoaringParallel.join(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime12 = crrTime12 / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, Iterative Optimization, " + crrTime12.toString + "\n"
    text


  def experiment(): Unit = {
    val exp = loadJoin()
    val expFunc = loadFunctional()
    val pw = PrintWriter(resultFilename)
    pw.println("FigureName,IndexType,ExecutionMode,Optimization,Performance")
    exp.foreach { case (db, query, positives, name) => {
      println("Experimenting started: "+name)
      pw.println(experiment(db, query, name))
      println("Experimenting finished for "+name)
    }}

    expFunc.foreach { case (db, query, positives, name) => {
      println("Experimenting started: "+name)
      pw.println(experimentFunctional(db, query, positives, name))
      println("Experimenting finished for "+name)
    }}


    pw.close()
  }

  def main(args: Array[String]): Unit = {
    experiment()
  }
}
