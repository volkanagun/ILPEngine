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
  val joinExperiments = Array("ptc","pte","acetyl","dunnhumby1","iggp", "imdb", "kinship", "protein", "random0","random1","random2",  "noisy","suranim","trains1", "trains2", "uwcs","webkb","zendo", "yeast")
  //val joinExperiments = Array("ptc","pte","acetyl","dunnhumby1","iggp", "imdb", "kinship", "protein", "random0","random1","random2",  "noisy","suranim","trains1", "trains2", "uwcs","webkb","zendo", "yeast")
  //val joinExperiments = Array("yeast")
  val functionalExperiments = Array("robots-functional","robots-linear")
  val resultFilename = "resources/experiments/performance.csv"

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

    val engine = Engine(database)
    val plan = Plan(database)
    val hypothesis = query
    val optimizedNone = plan.optimizeNone(hypothesis)
    val optimizedBellman = plan.optimizeBellmanFord(hypothesis)
    val optimizedExperimental = plan.optimizeExperimental(hypothesis)


    var crrTime = measureMultipleTime({
      engine.joinSerial(optimizedNone, Substitution())
    })

    text = text + s"${name}, No Index, Serial, No Optimization," + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinSerial(optimizedBellman, Substitution())
    })

    text = text + s"${name}, No Index, Serial, Bellmanford Optimization," + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinSerial(optimizedExperimental, Substitution())
    })

    text = text + s"${name}, No Index, Serial, Iterative Optimization," + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinParallel(optimizedNone, Substitution())
    })

    text = text + s"${name}, No Index, Parallel, No Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinParallel(optimizedBellman, Substitution())
    })

    text = text + s"${name}, No Index, Parallel, BellmanFord Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinParallel(optimizedExperimental, Substitution())
    })

    text = text + s"${name}, No Index, Parallel, Iterative Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinRoaringSerial(optimizedNone, Substitution())
    })

    text = text + s"${name}, Roaring Index, Serial, No Optimization, " + crrTime.toString + "\n"


    crrTime = measureMultipleTime({
      engine.joinRoaringSerial(optimizedBellman, Substitution())
    })

    text = text + s"${name}, Roaring Index, Serial, BellmanFord Optimization, " + crrTime.toString + "\n"


    crrTime = measureMultipleTime({
      engine.joinRoaringSerial(optimizedExperimental, Substitution())
    })

    text = text + s"${name}, Roaring Index, Serial, Iterative Optimization, " + crrTime.toString + "\n"


    crrTime = measureMultipleTime({
      engine.joinRoaringParallel(optimizedNone, Substitution())
    })

    text = text + s"${name}, Roaring Index, Parallel, No Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinRoaringParallel(optimizedBellman, Substitution())
    })

    text = text + s"${name}, Roaring Index, Parallel, BellmanFord Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      engine.joinRoaringParallel(optimizedExperimental, Substitution())
    })

    text = text + s"${name}, Roaring Index, Parallel, Iterative Optimization, " + crrTime.toString + "\n"
    println(text)
    text

  def experimentFunctional(database: Database, query: Hypothesis, predicates:Set[Predicate], name: String): String =
    var text = ""

    val engine = Engine(database)
    val plan = Plan(database)
    val hypothesis = query

    val optimizedNone = plan.optimizeNone(hypothesis)
    val optimizedRel = plan.optimizeBellmanFord(hypothesis)
    val optimizedExperimental = plan.optimizeExperimental(hypothesis)
    val hypothesisHead = optimizedNone.last.getHead()

    var crrTime = measureMultipleTime({
      predicates.foreach(predicate=>{
        engine.joinSerial(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, No Index, Serial, No Optimization," + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.foreach(predicate => {
        engine.joinSerial(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, No Index, Serial, BellmanFord Optimization," + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.foreach(predicate => {
        engine.joinSerial(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, No Index, Serial, Iterative Optimization," + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engine.joinParallel(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, No Index, Parallel, No Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine.joinParallel(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, No Index, Parallel, BellmanFord Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine.joinParallel(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, No Index, Parallel, Iterative Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.par.foreach(predicate=>{
        engine.joinRoaringParallel(optimizedNone, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, No Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine.joinRoaringParallel(optimizedRel, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, BellmanFord Optimization, " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      predicates.par.foreach(predicate=> {
        engine.joinRoaringParallel(optimizedExperimental, predicate.toSubstitution(hypothesisHead))
      })
    })

    crrTime = crrTime / predicates.size
    text = text + s"${name}, Roaring Index, Parallel, Iterative Optimization, " + crrTime.toString + "\n"
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
