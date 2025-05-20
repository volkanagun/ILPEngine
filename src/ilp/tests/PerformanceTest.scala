package ilp.tests

import ilp.data.database.{Database, Engine, Plan}
import ilp.data.predicates.Predicate
import ilp.data.{Hypothesis, Parser, Substitution}

import java.io.{File, PrintWriter}
import java.util.regex.Pattern
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.io.Source

object PerformanceTest {
  val folder = "examples/"
  val experiments = Array("kinship","imdb","zendo1","dunnhumby1","dunnhumby2")
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

  def loadDatabase(filename: String): Database =
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
      .map(line => {
        val rule = Parser.parseRule(line).get
        rule
      }).toSet

    Hypothesis(rules)

  def load(): Array[(Database, Hypothesis, Set[Predicate], String)] = {
    val files = File(folder).listFiles().filter(file => file.isDirectory)
      .filter(file => experiments.exists(starting => file.getName.startsWith(starting)))
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

  def initialize(hypothesis: Hypothesis, instances: Set[Predicate], limit: Int = 5): Hypothesis = {
    val allQuery = instances.take(limit).flatMap(predicate => hypothesis.getSorted().map(rule => rule.call(predicate).toRule()))
    Hypothesis(hypothesis.getHead(), allQuery)
  }

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


  def experiment(database: Database, query: Hypothesis, instances: Set[Predicate], name: String): String =
    var text = ""

    val engine = Engine(database)
    val plan = Plan(database)
    val hypothesis = query
    val optimizedNone = plan.optimizeNone(hypothesis)
    val optimizedRel = plan.optimizeRelative(hypothesis)
    var crrTime = measureMultipleTime({
      val set = engine.joinCyclic(optimizedNone, Substitution())
      //println(s"Correct: ${test(set, instances)}")
    }, 5)

    text = text + name + "\n"
    text = text + "No index, no optimization : " + crrTime.toString + "\n"


    crrTime = measureMultipleTime({
      val set = engine.joinCyclic(optimizedRel, Substitution())
    }, 5)

    text = text + "No index, relative optimization : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      val set = engine.joinCyclicParallel(optimizedNone, Substitution())

    }, 5)

    text = text + "No index, parallel, no optimization, cache : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      val trie = TrieMap[String, Set[Substitution]]()
      val set = engine.joinCyclicParallel(optimizedRel, Substitution())
    }, 5)

    text = text + "No index, parallel, relative optimization, cache : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      val set = engine.joinCyclicRoaring(optimizedNone, Substitution())
    }, 5)

    text = text + "Index, parallel, no optimization, roaring : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      val set = engine.joinCyclicRoaring(optimizedRel, Substitution())
    }, 5)

    text = text + "Index, parallel, relative optimization, roaring : " + crrTime.toString + "\n"

    crrTime = measureMultipleTime({
      val set = engine.joinCyclicBitmap(optimizedRel, Substitution())
    }, 5)

    text = text + "Index, parallel, relative optimization, bitmap : " + crrTime.toString + "\n"
    text


  def experiment(): Unit = {
    val exp = load()
    val pw = PrintWriter(resultFilename)
    exp.foreach { case (db, query, positives, name) => {
      println("Experimenting : " + name)

      pw.println(experiment(db, query, positives, name))
      pw.println()
    }
    }
    pw.close()
  }

  def main(args: Array[String]): Unit = {
    experiment()
  }
}
