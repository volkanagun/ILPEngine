package ilp.experiments

import ilp.data.database.{Database, Engine}
import ilp.data.optimization.Plan
import ilp.data.predicates.Predicate
import ilp.data.{Hypothesis, Parser, Substitution}

import java.io.{File, PrintWriter}
import java.util.regex.Pattern
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.io.Source

object Performance {
  val folder = "examples/"
  //val joinExperiments = Array("ptc","pte","acetyl","dunnhumby1","iggp", "imdb", "kinship", "protein", "random0","random1","random2",  "noisy","suranim","trains1", "trains2", "uwcs","webkb","zendo", "yeast")
  val joinExperiments = Array("iggp")
  //val joinExperiments = Array("yeast")
  val functionalExperiments = Array("robots-functional","robots-linear")
  val resultFilename = "resources/experiments/performance.csv"

  def measureMultipleTime[T](block: => T, count: Int = 1): (T, Double) = {

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
          val rule = Parser.parseRule(line).get
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

  def test(foundSet: Set[Substitution], instances: Set[Predicate]): Boolean = {
    val unMatched = instances.filter(predicate => {

      val substitution = predicate.toSubstitution()
      val contains = foundSet.exists(found => found.contains(substitution))
      !contains
    })

    unMatched.nonEmpty
  }


  def experiment(database: Database, query: Hypothesis, name: String): String =
    var text = ""

    val engine1 = Engine(database)
    val plan = Plan(database)
    val hypothesis = query
    val optimizedNone = plan.optimizeNone(hypothesis)
    val optimizedBellman = plan.optimizeBellmanFord(hypothesis)
    val optimizedExperimental = plan.optimizeExperimental(hypothesis)

    val (result1, crrTime1)  = measureMultipleTime({
      engine1.joinSerial(optimizedNone, Substitution())
    })

    println("No Index, Serial, No Optimization, Count:"+result1.size)
    text = text + s"${name}, No Index, Serial, No Optimization," + crrTime1.toString + "\n"
    val engine2 = Engine(database)
    val (result2, crrTime2) = measureMultipleTime({
      engine2.joinSerial(optimizedBellman, Substitution())
    })

    println("No Index, Serial, Bellmanford Optimization, Count:" + result2.size)
    text = text + s"${name}, No Index, Serial, Bellmanford Optimization," + crrTime2.toString + "\n"

    val engine3 = Engine(database)
    val (result3, crrTime3) = measureMultipleTime({
      engine3.joinSerial(optimizedExperimental, Substitution())
    })

    println("No Index, Serial, Iterative Optimization, Count:" + result3.size)
    text = text + s"${name}, No Index, Serial, Iterative Optimization," + crrTime3.toString + "\n"

    val engine4 = Engine(database)
    val (result4, crrTime4) = measureMultipleTime({
      engine4.joinParallel(optimizedNone, Substitution())
    })

    println("No Index, Parallel, No Optimization, Count:" + result4.size)
    text = text + s"${name}, No Index, Parallel, No Optimization, " + crrTime4.toString + "\n"

    val engine5 = Engine(database)
    val (result5, crrTime5) = measureMultipleTime({
      engine1.joinParallel(optimizedBellman, Substitution())
    })

    println("No Index, Parallel, Bellmanford Optimization, Count:" + result5.size)
    text = text + s"${name}, No Index, Parallel, BellmanFord Optimization, " + crrTime5.toString + "\n"

    val engine6 = Engine(database)

    val (result6, crrTime6) = measureMultipleTime({
      engine6.joinParallel(optimizedExperimental, Substitution())
    })

    println("No Index, Parallel, Iterative Optimization, Count:" + result6.size)
    text = text + s"${name}, No Index, Parallel, Iterative Optimization, " + crrTime6.toString + "\n"

    val engine7 = Engine(database)

    val (result7, crrTime7) = measureMultipleTime({
      engine7.joinRoaringSerial(optimizedNone, Substitution())
    })

    println("Roaring Index, Serial, No Optimization, Count:" + result7.size)
    text = text + s"${name}, Roaring Index, Serial, No Optimization, " + crrTime7.toString + "\n"


    val engine8 = Engine(database)
    val (result8, crrTime8) = measureMultipleTime({
      engine8.joinRoaringSerial(optimizedBellman, Substitution())
    })

    println("Roaring Index, Serial, Bellmanford Optimization, Count:" + result8.size)
    text = text + s"${name}, Roaring Index, Serial, BellmanFord Optimization, " + crrTime8.toString + "\n"


    val engine9 = Engine(database)
    val (result9, crrTime9) = measureMultipleTime({
      engine9.joinRoaringSerial(optimizedExperimental, Substitution())
    })

    println("Roaring Index, Serial, Iterative Optimization, Count:" + result9.size)
    text = text + s"${name}, Roaring Index, Serial, Iterative Optimization, " + crrTime9.toString + "\n"


    val engine10 = Engine(database)
    val (result10, crrTime10) = measureMultipleTime({
      engine10.joinRoaringParallel(optimizedNone, Substitution())
    })

    println("Roaring Index, Parallel, No Optimization, Count:" + result10.size)
    text = text + s"${name}, Roaring Index, Parallel, No Optimization, " + crrTime10.toString + "\n"

    val engine11 = Engine(database)
    val (result11, crrTime11) = measureMultipleTime({
      engine11.joinRoaringParallel(optimizedBellman, Substitution())
    })

    println("Roaring Index, Parallel, Bellmanford Optimization, Count:" + result11.size)
    text = text + s"${name}, Roaring Index, Parallel, BellmanFord Optimization, " + crrTime11.toString + "\n"

    val engine12 = Engine(database)
    val (result12, crrTime12) = measureMultipleTime({
      engine12.joinRoaringParallel(optimizedExperimental, Substitution())
    })

    println("Roaring Index, Parallel, Iterative Optimization, Count:" + result12.size)
    text = text + s"${name}, Roaring Index, Parallel, Iterative Optimization, " + crrTime12.toString + "\n"
    println(text)
    text

  def experimentFunctional(database: Database, query: Hypothesis, predicates:Set[Predicate], name: String): String =
    var text = ""

    val engine1 = Engine(database)
    val plan = Plan(database)
    val hypothesis = query

    val optimizedNone = plan.optimizeNone(hypothesis)
    val optimizedRel = plan.optimizeBellmanFord(hypothesis)
    val optimizedExperimental = plan.optimizeExperimental(hypothesis)
    val hypothesisHead = optimizedNone.last.getHead()

    var (result1, crrTime1) = measureMultipleTime({
      predicates.foreach(predicate=>{
        engine1.joinSerial(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime1 = crrTime1 / predicates.size
    text = text + s"${name}, No Index, Serial, No Optimization," + crrTime1.toString + "\n"

    val engine2 = Engine(database)
    var (result2, crrTime2) = measureMultipleTime({
      predicates.foreach(predicate => {
        engine2.joinSerial(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime2 = crrTime2 / predicates.size
    text = text + s"${name}, No Index, Serial, BellmanFord Optimization," + crrTime2.toString + "\n"
    val engine3 = Engine(database)
    var (result3, crrTime3) = measureMultipleTime({
      predicates.foreach(predicate => {
        engine3.joinSerial(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime3 = crrTime3 / predicates.size
    text = text + s"${name}, No Index, Serial, Iterative Optimization," + crrTime3.toString + "\n"

    val engine4 = Engine(database)
    var(result4, crrTime4) = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engine4.joinParallel(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime4 = crrTime4 / predicates.size
    text = text + s"${name}, No Index, Parallel, No Optimization, " + crrTime4.toString + "\n"
    val engine5 = Engine(database)
    var (result5, crrTime5) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine5.joinParallel(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime5 = crrTime5 / predicates.size
    text = text + s"${name}, No Index, Parallel, BellmanFord Optimization, " + crrTime5.toString + "\n"
    val engine6 = Engine(database)
    var (result6, crrTime6) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine6.joinParallel(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime6 = crrTime6 / predicates.size
    text = text + s"${name}, No Index, Parallel, Iterative Optimization, " + crrTime6.toString + "\n"
    val engine7 = Engine(database)
    var (result7, crrTime7) = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engine7.joinRoaringParallel(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime7 = crrTime7 / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, No Optimization, " + crrTime7.toString + "\n"
    val engine8 = Engine(database)
    var (result8, crrTime8) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine8.joinRoaringParallel(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime8 = crrTime8 / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, BellmanFord Optimization, " + crrTime8.toString + "\n"
    val engine9 = Engine(database)
    var (result9, crrTime9) = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine9.joinRoaringParallel(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime9 = crrTime9 / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, Iterative Optimization, " + crrTime9.toString + "\n"
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
