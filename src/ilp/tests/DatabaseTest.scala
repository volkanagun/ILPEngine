package ilp.tests

import ilp.data.database.{Database, Engine, Plan}
import ilp.data.variables.{Num, Variable}
import ilp.data.{Hypothesis, Parser, Substitution}
import ilp.experiments.{Experiment, Params}

import scala.collection.concurrent.TrieMap

object DatabaseTest {

  def simpleExecution(): Unit = {
    val db = Database("executionTest");
    val g1 = Parser.parsePredicate("g(4).").get
    val r = Parser.parseRule("f(X,Y) :- Y=X+1, g(Y).").get
    val h = Hypothesis(r.getHead(), r)
    val substitution = Substitution().add(Variable("X"), Num("X", 3.0))

    db.add(g1).build()
    val engine = Engine(db)
    val plan = Plan(db)
    val o1 = plan.optimizeExperimental(h)
    val results = engine.joinParallel(o1, substitution)
    println(results.size)
  }

  def simpleMixCycling(): Unit = {
    val params = Params("synthesis-length")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()
    val hypothesis = experiment.getHypothesis()
    val positives = experiment.getPositives()

    val engine = Engine(db, recursiveDepth = 8)
    val plan = Plan(db)

    val optimizedList = plan.optimizeMinMin(hypothesis)

    positives.foreach(positive => {
      val substitution = hypothesis.substitution(positive)
      val results = engine.joinParallel(optimizedList, substitution)
      println("Has result: " +results.nonEmpty)
      println(results)
    })
  }

  def simpleIMDB():Unit= {
    val params = Params("imdb1")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()

    val engine = Engine(db, 5)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("h95(A1) :- actor(A1).\n" +
      "k516(A2) :- director(A2).\n" +
      "k721(M1,A3) :- movie(M1,A3).\n" +
      "c131(M2,A4) :- h95(A4) & k721(M2,A4).\n" +
      "g757(M3,D1) :- k516(D1) & k721(M3,D1).\n" +
      "l131(D2,A5) :- g757(M1,D2) & c131(M1,A5).").get

    val optimizedList = plan.optimizeExperimental(hypothesis)
    val results = engine.joinParallel(optimizedList, Substitution())
    println("Result size: " +results.size)
  }

  def simpleCyclicIMDB():Unit= {
    val params = Params("imdb1")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()

    val engine = Engine(db, 5)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("h95(A1) :- actor(A1).\n" +
      "k516(A2) :- director(A2).\n" +
      "k721(M1,A3) :- movie(M1,A3).\n" +
      "c131(M2,A4) :- h95(A4) & k721(M2,A4).\n" +
      "g757(M3,D1) :- k516(D1) & k721(M3,D1).\n" +
      "l131(D2,A5) :- g757(M1,D2) & c131(M1,A5).").get

    val optimizedList = plan.optimizeExperimental(hypothesis)
    val results = engine.joinCyclicBottomUp(optimizedList,Substitution())
    println("Result size: " +results.size)
  }

  def simpleParallelTrains():Unit= {
    val params = Params("trains1")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()

    val engine = Engine(db, 3)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("f(V0):- has_car(V0,V2),three_wheels(V2),has_car(V0,V1),long(V1),roof_closed(V1).").get

    val optimizedList1 = plan.optimizeMinMin(hypothesis)
    val optimizedList2 = plan.optimizeExperimental(hypothesis)
    val result1 = engine.joinParallel(optimizedList1,Substitution())
    println(s"Result1: ${result1.size}")
    val result2 = engine.joinParallel(optimizedList2,Substitution())
    println(s"Result2: ${result2.size}")
    println("Check: " + (result1.size == result2.size))
  }

  def roaringCycling(): Unit = {
    val params = Params("synthesis-length")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()
    val hypothesis = experiment.getHypothesis()
    val positives = experiment.getPositives()

    val engine = Engine(db, recursiveDepth = 8)
    val plan = Plan(db)

    val optimizedList = plan.optimizeMinMin(hypothesis)

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
    simpleParallelTrains()
  }

}