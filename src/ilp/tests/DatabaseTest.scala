package ilp.tests

import ilp.data.database.{Database, Engine, Plan}
import ilp.data.variables.{Num, Variable}
import ilp.data.{Parser, Substitution}
import ilp.experiments.{Experiment, Params}

import scala.collection.concurrent.TrieMap

object DatabaseTest {

  def simpleExecution(): Unit = {
    val db = Database("executionTest");
    val g1 = Parser.parsePredicate("g(4).").get
    val f = Parser.parseRule("f(X,Y) :- Y=X+1, g(Y).").get
    val main = Substitution().add(Variable("X"), Num("X", 3.0))

    db.add(g1)
    val engine = Engine(db)
    val plan = Plan(db)
    val o1 = plan.optimizeRelative(f)

    engine.execute(o1, main).getVariables()
      .foreach(variable=>println(variable.toString))
  }

  def simpleMixCycling(): Unit = {
    val params = Params("synthesis-length")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()
    val hypothesis = experiment.getHypothesis()
    val positives = experiment.getPositives()

    val engine = Engine(db, recursiveDepth = 8)
    val plan = Plan(db)

    val optimizedList = plan.optimizeRelative(hypothesis)

    positives.foreach(positive => {
      val substitution = hypothesis.substitution(positive)
      val cache = TrieMap[Int, Set[Substitution]]()
      val results = engine.joinCyclicParallel(cache, optimizedList, substitution)
      println("Has result: " +results.nonEmpty)
      println(results)
    })


  }
  def roaringCycling(): Unit = {
    val params = Params("synthesis-length")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()
    val hypothesis = experiment.getHypothesis()
    val positives = experiment.getPositives()

    val engine = Engine(db, recursiveDepth = 8)
    val plan = Plan(db)

    val optimizedList = plan.optimizeRelative(hypothesis)

    positives.foreach(positive => {
      val substitution = hypothesis.substitution(positive)
      val cache = TrieMap[Int, Set[Substitution]]()
      val results = engine.joinCyclicRoaring(optimizedList, substitution)
      println("Has result: " +results.nonEmpty)
      println(results)
    })


  }

  def simpleCycling(): Unit = {

    val db = Database("recursiveTest");
    val p1 = Parser.parsePredicate("f(5, 1).").get
    val p2 = Parser.parsePredicate("f(4, 1).").get
    val p3 = Parser.parsePredicate("f(3, 2).").get
    val p4 = Parser.parsePredicate("f(2, 3).").get
    val g1 = Parser.parsePredicate("g(3).").get
    val g2 = Parser.parsePredicate("g(4).").get
    val g3 = Parser.parsePredicate("g(5).").get

    db.add(p1)
      .add(p2)
      .add(p3)
      .add(p4)
      .add(g1)
      .add(g2)
      .add(g3)
      .build()

    val engine = Engine(db)
    val plan = Plan(db)

    val s1 = Substitution().add(Variable("X"), Num("X", 6))
      .add(Variable("Y"), Num("Y", 2))

    val r1 = Parser.parseRule("f(X, Y) :- g(X), X=X-1, f(X,Y).").get
    val query = plan.optimizeRelative(r1)
    val queries=Array(query)
    val substitution = Substitution()
    val substitutions = engine.joinCyclic(queries, substitution)
    substitutions.foreach(sub=> println(sub))
  }

  def main(args: Array[String]): Unit = {
    roaringCycling()

  }

}