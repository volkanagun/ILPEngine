package ilp.tests

import ilp.data.database.{Database, Engine, Plan}
import ilp.data.variables.{Num, Variable}
import ilp.data.{Hypothesis, Parser, Substitution}
import ilp.experiments.{Experiment, Params}

import scala.collection.concurrent.TrieMap

object DatabaseTest {

  def simpleExecution(): Unit = {
    val db = Database("executionTest")
    val g1 = Parser.parsePredicate("g(4).").get
    val r = Parser.parseRule("f(X,Y) :- Y=X+1, g(Y).").get
    val h = Hypothesis(r.getHead(), r)
    val substitution = Substitution().add(Variable("X"), Num("X", 1.0))

    db.add(g1).build()
    val engine = Engine(db)
    val plan = Plan(db)
    val o1 = plan.optimizeExperimental(h)
    val results = engine.joinParallel(o1, substitution)
    println(results.mkString("[",",","]"))
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
    //val resultBottomup = engine.joinCyclicBottomUp(optimizedList,Substitution())
    val resultParallel = engine.joinParallel(optimizedList,Substitution())
    val resultRoaring = engine.joinRoaringParallel(optimizedList,Substitution())

    //println("Result bottomup size: " +resultBottomup.size)
    println("Result parallel size: " +resultParallel.size)
    println("Result roaring size: " +resultRoaring.size)
  }

  private def simpleParallelTrains():Unit= {
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

  def simpleFunctional(): Unit = {

    val main = Set(Variable("A"))
    val other = Set(Num("A", 78).asVariable())
    val all = Set(other, main)
    val ii = other.intersect(main)

    val params = Params("robots-functional")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()
    val hypothesis = experiment.getHypothesis()
    val positives = experiment.getPositives()

    val engine = Engine(db, recursiveDepth = 5)
    val plan = Plan(db)

    for(positive <- positives) {
      val program = plan.optimizeExperimental(hypothesis)
      val parallelResults = engine.joinParallel(program, positive.toSubstitution(hypothesis.getHead()))
      val roaringResults = engine.joinRoaringParallel(program, positive.toSubstitution(hypothesis.getHead()))
      println(s"Predicate: ${positive}, Parallel Has result: "+parallelResults.nonEmpty)
      println(s"Predicate: ${positive}, Roaring Has result: "+roaringResults.nonEmpty)
    }
  }

  def simpleFunctionalTime(): Unit = {

    val main = Set(Variable("A"))
    val other = Set(Num("A", 78).asVariable())
    val all = Set(other, main)
    val ii = other.intersect(main)

    val params = Params("robots-linear")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase()
    val hypothesis = experiment.getHypothesis()
    val positives = experiment.getPositives()

    val engine = Engine(db, recursiveDepth = 5)
    val plan = Plan(db)
    val program = plan.optimizeNone(hypothesis)
    var tParallel = 0L
    var tRoaring = 0L
    val filterSubstitution = Substitution()
      .add(Variable("A"), Variable("X0"))
      .add(Variable("B"), Variable("Y0"))
      .add(Variable("C"), Variable("X1"))
      .add(Variable("D"), Variable("Y1"))



    for(positive <- positives) {
      val newHead = filterSubstitution.filterReplace(positive)
      val beginParallel = System.nanoTime()
      //Set input variables
      program.foreach(optimized => optimized.query.setInputVariables(optimized.query.inputVariables.slice(0, 2)))
      val parallelResults = engine.joinParallel(program, newHead)
      tParallel += System.nanoTime() - beginParallel
      val beginRoaring = System.nanoTime()
      val roaringResults = engine.joinRoaringParallel(program, newHead)
      tRoaring += System.nanoTime() - beginRoaring
      println(s"Predicate: ${positive}, Parallel Has result: "+parallelResults.nonEmpty)
      println(s"Predicate: ${positive}, Roaring Has result: "+roaringResults.nonEmpty)
    }

    println("====================================")
    println("Parallel: " + tParallel.toDouble/10000000)
    println("Roaring: " + tRoaring.toDouble/10000000)

  }

  def simpleRecursive(): Unit = {

    val db = Database("recursiveTest")
    val p1 = Parser.parsePredicate("f(5, 1).").get
    val p2 = Parser.parsePredicate("f(4, 1).").get
    val p3 = Parser.parsePredicate("f(3, 2).").get
    val p4 = Parser.parsePredicate("f(2, 3).").get
    val p5 = Parser.parsePredicate("f(2, 2).").get
    val g0 = Parser.parsePredicate("g(2).").get
    val g1 = Parser.parsePredicate("g(3).").get
    val g2 = Parser.parsePredicate("g(4).").get
    val g3 = Parser.parsePredicate("g(5).").get

    db.add(p1)
      .add(p2)
      .add(p3)
      .add(p4)
      .add(p5)
      .add(g0)
      .add(g1)
      .add(g2)
      .add(g3)
      .build()

    val engine = Engine(db)
    val plan = Plan(db)

    val substitution = Substitution().add(Variable("X"), Num("X", 5))


    val r1 = Parser.parseHypothesis("f(X, Y) :- g(X1), X1=X-1, f(X1,Y).").get
    val queries = plan.optimizeExperimental(r1)

    val parallelSubstitutions = engine.joinParallel(queries, substitution)
    println("Parallel result: ")
    parallelSubstitutions.foreach(sub=> println(sub))
    println("===========================================")

    val roaringSubstitutions = engine.joinRoaringParallel(queries, substitution)
    println("Roaring result: ")
    roaringSubstitutions.foreach(sub=> println(sub))
    println("===========================================")
  }

  def main(args: Array[String]): Unit = {
    simpleFunctionalTime()
  }

}