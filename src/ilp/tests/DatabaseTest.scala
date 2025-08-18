package ilp.tests

import ilp.data.database.{Database, EngineParallel, EngineRoaringParallel, EngineRoaringSerial, EngineSerial}
import ilp.data.optimization.Plan
import ilp.data.program.{Hypothesis, Parser, Substitution}
import ilp.data.variables.{Num, Sym, Variable, VariableList}
import ilp.experiments.{Experiment, Params}
import ilp.invent.{Binary, BinaryFunctional}

import scala.collection.concurrent.TrieMap

object DatabaseTest {

  def simpleExecution(): Unit = {
    val db = Database("executionTest")
    val g1 = Parser.parsePredicate("g(4).").get
    val r = Parser.parseRule("f(X,Y) :- Y=X+1, g(Y).").get
    val h = Hypothesis(r.getHead, r)
    val substitution = Substitution().add(Variable("X"), Num("X", 1.0))

    db.add(g1).build()
    val engine = EngineParallel(db, 5)
    val plan = Plan(db)
    val o1 = plan.optimizeExperimental(h)
    val results = engine.join(o1, substitution)
    println(results.mkString("[",",","]"))
  }


  def simpleMixCycling(): Unit = {
    val params = Params("synthesis-length")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase
    val hypothesis = experiment.getHypothesis
    val positives = experiment.getPositives

    val engine = EngineParallel(db, depth = 8)
    val plan = Plan(db)

    val optimizedList = plan.optimizeExperimental(hypothesis)

    positives.foreach(positive => {
      val substitution = hypothesis.substitution(positive)
      val results = engine.join(optimizedList, substitution)
      println("Has result: " +results.nonEmpty)
      println(results)
    })
  }

  def simpleIMDB():Unit= {
    val params = Params("imdb1")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val engine = EngineParallel(db, 5)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("h95(A1) :- actor(A1).\n" +
      "k516(A2) :- director(A2).\n" +
      "k721(M1,A3) :- movie(M1,A3).\n" +
      "c131(M2,A4) :- h95(A4) & k721(M2,A4).\n" +
      "g757(M3,D1) :- k516(D1) & k721(M3,D1).\n" +
      "l131(D2,A5) :- g757(M1,D2) & c131(M1,A5).").get

    val optimizedList = plan.optimizeExperimental(hypothesis)
    val results = engine.join(optimizedList, Substitution())
    println("Result size: " +results.size)
  }

  def simpleZendeo():Unit= {
    val params = Params("zendo2")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val engineSerial = EngineSerial(db, 5)
    val engineRoaringParallel = EngineRoaringParallel(db, 5)
    val engineParallel = EngineParallel(db, 5)
    val plan = Plan(db)
    val line = "func591418883(V0,V1) :- piece(V0,V1) & green(V1).\n"+
      "func1815986585(V0,V1) :- piece(V0,V1) & red(V1).\n"+
      "func688690204(V0,V1) :- piece(V0,V1) & blue(V1).\n"+
      "func1854231869(V0,V1) :- piece(V0,V1) & lhs(V1).\n"+
      "func1890519296(V1,V2,V3) :- coord1(V1,V3) & coord1(V2,V3).\n"+
      //"func1086357946(V0) :- func591418883(V0,V1) & func1815986585(V0,V2) & func688690204(V0,V3).\n"+
      "func1086357946(V0) :- func591418883(V0,V1) & func1854231869(V0,V2) & func1890519296(V1,V2,V3)."

    val l1Norm = "func1086357946(V0) :- piece(V0,V1) & green(V1) & piece(V0,V2) & lhs(V2) & coord1(V1,V3) & coord1(V2,V3)."


    val hypothesis = Parser.parseHypothesis(line).get.build().compact()
    val hnorm = Parser.parseHypothesis(l1Norm).get

    val optimizedList1 = plan.optimizeExperimental(hypothesis)
    val optimizedList2 = plan.optimizeExperimental(hypothesis)
    val optimizedList3 = plan.optimizeExperimental(hnorm)

    val results1 = engineRoaringParallel.join(optimizedList1, Substitution())
    val results2 = engineSerial.join(optimizedList2, Substitution())
    val results3 = engineParallel.join(optimizedList2, Substitution())
    val results4 = engineSerial.join(optimizedList3, Substitution())

    println(hypothesis)
    println(hnorm)

    println("Result roaaring size: " +results1.size)
    println("Result serial size: " +results2.size)
    println("Result parallel size: " +results3.size)
    println("Result norm size: " +results4.size)
    //println("Normalized size: " +results2.size)
  }

  def simpleCyclicIMDB():Unit= {
    val params = Params("imdb1")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val engine = EngineSerial(db, 5)
    val engineParallel = EngineParallel(db, 5)
    val engineRoaringParallel = EngineRoaringParallel(db, 5)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("h95(A1) :- actor(A1).\n" +
      "k516(A2) :- director(A2).\n" +
      "k721(M1,A3) :- movie(M1,A3).\n" +
      "c131(M2,A4) :- h95(A4) & k721(M2,A4).\n" +
      "g757(M3,D1) :- k516(D1) & k721(M3,D1).\n" +
      "l131(D2,A5) :- g757(M1,D2) & c131(M1,A5).").get

    val optimizedList = plan.optimizeExperimental(hypothesis)
    val resultParallel = engineParallel.join(optimizedList,Substitution())
    val resultRoaring = engineRoaringParallel.join(optimizedList,Substitution())

    println("Result parallel size: " +resultParallel.size)
    println("Result roaring size: " +resultRoaring.size)
  }

  private def simpleTrains3():Unit= {
    val params = Params("trains3-toy")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val engine = EngineSerial(db, 3)
    val engineParallel = EngineParallel(db, 3)
    val engineRoaringParallel = EngineRoaringParallel(db, 3)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("f(V0):- has_car(V0,V2),roof_open(V2),has_car(V0,V1),roof_closed(V1).\n"+
      "f(V0):- has_car(V0,V1),roof_open(V1),has_load(V1,V2),three_load(V2).\n"+
      "f(V0):- has_car(V0,V4),has_load(V4,V2),rectangle(V2),has_car(V0,V1),has_load(V1,V3),triangle(V3).").get

    val optimizedList1 = plan.optimizeNone(hypothesis)
    val optimizedList2 = plan.optimizeExperimental(hypothesis)
    val result1 = engineParallel.join(optimizedList1,Substitution())
    println(s"Result1: ${result1.size}")
    val result2 = engineParallel.join(optimizedList2,Substitution())
    println(s"Result2: ${result2.size}")
    println("Check: " + (result1.size == result2.size))
  }

  private def simpleParallelTrains():Unit= {
    val params = Params("trains1")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val engine = EngineSerial(db, 3)
    val engineParallel = EngineParallel(db, 3)
    val engineRoaringParallel = EngineRoaringParallel(db, 3)
    val plan = Plan(db)
    val hypothesis = Parser.parseHypothesis("f(V0):- has_car(V0,V2),three_wheels(V2),has_car(V0,V1),long(V1),roof_closed(V1).").get

    val optimizedList1 = plan.optimizeNone(hypothesis)
    val optimizedList2 = plan.optimizeExperimental(hypothesis)
    val result1 = engineParallel.join(optimizedList1,Substitution())
    println(s"Result1: ${result1.size}")
    val result2 = engineParallel.join(optimizedList2,Substitution())
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
    val db = experiment.getDatabase
    val hypothesis = experiment.getHypothesis
    val positives = experiment.getPositives

    val engine = EngineSerial(db, recursiveDepth = 5)
    val engineParallel = EngineParallel(db, 5)
    val engineRoaringParallel = EngineRoaringParallel(db, 5)

    val plan = Plan(db)

    for(positive <- positives) {
      val program = plan.optimizeExperimental(hypothesis)
      val parallelResults = engineParallel.join(program, positive.toSubstitution(hypothesis.getHead))
      val roaringResults = engineRoaringParallel.join(program, positive.toSubstitution(hypothesis.getHead))
      println(s"Predicate: $positive, Parallel Has result: "+parallelResults.nonEmpty)
      println(s"Predicate: $positive, Roaring Has result: "+roaringResults.nonEmpty)
    }
  }

  def simpleFunctionalTime(): Unit = {

    val main = Set(Variable("A"))
    val other = Set(Num("A", 78).asVariable())
    val all = Set(other, main)
    val ii = other.intersect(main)

    val params = Params("robots-functional")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase
    val hypothesis = experiment.getHypothesis.build()
    val positives = experiment.getPositives

    val engine = EngineSerial(db, recursiveDepth = 5)
    val engineParallel = EngineParallel(db, 5)
    val engineRoaringParallel = EngineRoaringParallel(db, 5)


    val plan = Plan(db)
    val program = plan.optimizeExperimental(hypothesis)
    var tParallel = 0L
    var tRoaring = 0L
    val filterSubstitution = Substitution()
      .add(Variable("A"), Variable("X0"))
      .add(Variable("B"), Variable("Y0"))
      .add(Variable("C"), Variable("X1"))
      .add(Variable("D"), Variable("Y1"))



    for(positive <- positives) {
      val newHead = filterSubstitution.get(positive).toSubstitution(positive)
      val beginParallel = System.nanoTime()
      //Set input variables
      program.foreach(optimized => optimized.query.setInputVariables(optimized.query.inputVariables.slice(0, 2)))
      val parallelResults = engineParallel.join(program, newHead)
      tParallel += System.nanoTime() - beginParallel
      val beginRoaring = System.nanoTime()
      val roaringResults = engineRoaringParallel.join(program, newHead)
      tRoaring += System.nanoTime() - beginRoaring
      println(s"Predicate: $positive, Parallel Has result: "+parallelResults.nonEmpty)
      println(s"Predicate: $positive, Roaring Has result: "+roaringResults.nonEmpty)
    }

    println("====================================")
    println("Parallel: " + tParallel.toDouble/10000000)
    println("Roaring: " + tRoaring.toDouble/10000000)

  }

  private def simpleIGGP(): Unit = {
    val params = Params("iggp-buttons-next")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val query = Parser.parseHypothesis(
      "next(V0,V1):- my_succ(V2,V1),my_true(V0,V2).\n"+
      "next(V0,V1):- my_true(V0,V1),c_r(V1),does(V0,V3,V2),c_b(V2).\n" +
      "next(V0,V1):- my_true(V0,V1),c_r(V1),c_a(V2),does(V0,V3,V2).\n" +
      "next(V0,V1):- my_true(V0,V1),c_q(V1),c_a(V2),does(V0,V3,V2).\n" +
      "next(V0,V1):- my_true(V0,V1),c_p(V1),c_c(V2),does(V0,V3,V2).\n" +
      "next(V0,V1):- c_p(V1),not_my_true(V0,V1),c_a(V3),does(V0,V2,V3).\n"+
      "next(V0,V1):- c_p(V1),c_b(V3),does(V0,V2,V3),c_q(V4),my_true(V0,V4).\n"+
      "next(V0,V1):- c_r(V1),does(V0,V4,V3),c_c(V3),c_q(V2),my_true(V0,V2).\n"+
      "next(V0,V1):- c_q(V1),c_p(V2),my_true(V0,V2),c_b(V4),does(V0,V3,V4).\n"+
      "next(V0,V1):- c_q(V1),c_c(V4),does(V0,V3,V4),c_r(V2),my_true(V0,V2).").get


    val plan = Plan(db)
    val optimizedNone = plan.optimizeNone(query)
    val optimizedBellmanford = plan.optimizeBellmanFord(query)

    val engine1 = EngineSerial(db)
    val engine2 = EngineSerial(db)
    val engineParallel = EngineParallel(db, 30)
    val engine4 = EngineSerial(db)
    val engineRoaringSerial = EngineRoaringSerial(db, 30)
    val engine6 = EngineSerial(db)
    val r1 = engine1.join(optimizedNone)
    val r2 = engine2.join(optimizedBellmanford)
    val r3 = engineParallel.join(optimizedNone)
    val r4 = engineParallel.join(optimizedBellmanford)
    val r5 = engineRoaringSerial.join(optimizedNone)
    val r6 = engineRoaringSerial.join(optimizedBellmanford)

    println("Result1 size: "+r1.size)
    println("Result2 size: "+r2.size)
    println("Result3 size: "+r3.size)
    println("Result4 size: "+r4.size)
    println("Result5 size: "+r5.size)
    println("Result6 size: "+r6.size)
    println("r1==r2 : "+ (r1.size==r2.size).toString)
    println("r1==r3 : "+ (r1.size==r3.size).toString)
    println("r3==r4 : "+ (r3.size==r4.size).toString)
    println("r1==r4 : "+ (r1.size==r4.size).toString)
    println("r1==r5 : "+ (r1.size==r5.size).toString)
    println("r5==r6 : "+ (r5.size==r6.size).toString)
  }

  private def simpleIGGPSokoban(): Unit = {
    val params = Params("iggp-sokoban-goal")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val query = Parser.parseHypothesis("goal(V0,V1,V2):- agent_black(V1),score_100(V2),obj_obj1(V5),obj_obj2(V3),true_target(V0,V6,V4),true_at(V0,V6,V4,V3),true_at(V0,V4,V6,V5).").get


    val plan = Plan(db)
    val optimizedNone = plan.optimizeNone(query)
    val optimizedBellmanford = plan.optimizeBellmanFord(query)

    val engine1 = EngineSerial(db)
    val engine2 = EngineSerial(db)
    val engineParallel = EngineParallel(db, 30)
    val engine4 = EngineSerial(db)
    val engineRoaringSerial = EngineRoaringSerial(db, 30)
    val engine6 = EngineSerial(db)
    val r1 = engine1.join(optimizedNone)
    val r2 = engine2.join(optimizedBellmanford)
    val r3 = engineParallel.join(optimizedNone)
    val r4 = engineParallel.join(optimizedBellmanford)
    val r5 = engineRoaringSerial.join(optimizedNone)
    val r6 = engineRoaringSerial.join(optimizedBellmanford)

    println("Result1 size: "+r1.size)
    println("Result2 size: "+r2.size)
    println("Result3 size: "+r3.size)
    println("Result4 size: "+r4.size)
    println("Result5 size: "+r5.size)
    println("Result6 size: "+r6.size)
    println("r1==r2 : "+ (r1.size==r2.size).toString)
    println("r1==r3 : "+ (r1.size==r3.size).toString)
    println("r3==r4 : "+ (r3.size==r4.size).toString)
    println("r1==r4 : "+ (r1.size==r4.size).toString)
    println("r1==r5 : "+ (r1.size==r5.size).toString)
    println("r5==r6 : "+ (r5.size==r6.size).toString)
  }

  private def simplePTC(): Unit = {
    val params = Params("ptc")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val query = Parser.parseHypothesis(
      //"label(V0):- zn(V2),atom(V1,V0,V2).\n" +
      //"label(V0):- cu(V2),atom(V1,V0,V2).\n" +
      "label(V0):- c(V4),connected(V3,V5,V2),atom(V5,V0,V4),p(V1),atom(V3,V0,V1).").get
      //"label(V0):- connected(V3,V5,V2),atom(V5,V0,V4),p(V1),h(V4),atom(V3,V0,V1).").get


    val plan = Plan(db)
    val optimizedNone = plan.optimizeNone(query)
    val optimizedBellmanford = plan.optimizeBellmanFord(query)

    val engine1 = EngineSerial(db)
    val engine2 = EngineSerial(db)
    val engineParallel = EngineParallel(db, 30)
    val engine4 = EngineSerial(db)
    val engineRoaringSerial = EngineRoaringSerial(db, 30)
    val engine6 = EngineSerial(db)
    val r1 = engine1.join(optimizedNone)
    val r2 = engine2.join(optimizedBellmanford)
    val r3 = engineParallel.join(optimizedNone)
    val r4 = engineParallel.join(optimizedBellmanford)
    val r5 = engineRoaringSerial.join(optimizedNone)
    val r6 = engineRoaringSerial.join(optimizedBellmanford)

    println("Result1 size: "+r1.size)
    println("Result2 size: "+r2.size)
    println("Result3 size: "+r3.size)
    println("Result4 size: "+r4.size)
    println("Result5 size: "+r5.size)
    println("Result6 size: "+r6.size)
    println("r1==r2 : "+ (r1.size==r2.size).toString)
    println("r1==r3 : "+ (r1.size==r3.size).toString)
    println("r3==r4 : "+ (r3.size==r4.size).toString)
    println("r1==r4 : "+ (r1.size==r4.size).toString)
    println("r1==r5 : "+ (r1.size==r5.size).toString)
    println("r5==r6 : "+ (r5.size==r6.size).toString)
  }


  private def simpleYeast(): Unit = {
    val params = Params("yeast")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val query = Parser.parseHypothesis(
      "proteins(V0):- path(V0,V2),location(V0,V1).\n" +
      "proteins(V0):- enzyme(V0,V1),renzyme(V0,V1).\n" +
      "proteins(V0):- path(V2,V3),interaction(V3,V0,V1).\n" +
      "proteins(V0):- protein_class(V0,V1),rprotein_class(V0,V1).\n" +
      "proteins(V0):- protein_class(V0,V4),interaction(V2,V0,V1),rprotein_class(V3,V4).\n" +
      "proteins(V0):- phenotype(V0,V3),renzyme(V0,V2),rphenotype(V1,V3).\n" +
      "proteins(V0):- protein_class(V0,V3),rprotein_class(V2,V3),enzyme(V2,V1).\n" +
      "proteins(V0):- interaction(V3,V0,V1),protein_class(V3,V2),rprotein_class(V3,V2).\n" +
      "proteins(V0):- path(V2,V1),interaction(V2,V0,V3),rprotein_class(V0,V4).").get


    val plan = Plan(db)
    val optimizedNone = plan.optimizeNone(query)
    val optimizedBellmanford = plan.optimizeBellmanFord(query)

    val engine1 = EngineSerial(db)
    val engine2 = EngineSerial(db)
    val engineParallel = EngineParallel(db, 30)
    val engine4 = EngineSerial(db)
    val engineRoaringSerial = EngineRoaringSerial(db, 30)
    val engine6 = EngineSerial(db)
    val r1 = engine1.join(optimizedNone)
    val r2 = engine2.join(optimizedBellmanford)
    val r3 = engineParallel.join(optimizedNone)
    val r4 = engineParallel.join(optimizedBellmanford)
    val r5 = engineRoaringSerial.join(optimizedNone)
    val r6 = engineRoaringSerial.join(optimizedBellmanford)

    println("Result1 size: "+r1.size)
    println("Result2 size: "+r2.size)
    println("Result3 size: "+r3.size)
    println("Result4 size: "+r4.size)
    println("Result5 size: "+r5.size)
    println("Result6 size: "+r6.size)
    println("r1==r2 : "+ (r1.size==r2.size).toString)
    println("r1==r3 : "+ (r1.size==r3.size).toString)
    println("r3==r4 : "+ (r3.size==r4.size).toString)
    println("r1==r4 : "+ (r1.size==r4.size).toString)
    println("r1==r5 : "+ (r1.size==r5.size).toString)
    println("r5==r6 : "+ (r5.size==r6.size).toString)
  }


  private def simpleWebkb(): Unit = {
    val params = Params("webkb")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val query = Parser.parseHypothesis("faculty(V0):- courseprof(V1,V0),project(V4,V0),project(V4,V3),courseta(V2,V3).\n"+
      "faculty(V0):- courseprof(V5,V0),courseta(V5,V3),courseta(V4,V3),courseprof(V4,V2),project(V1,V2).").get


    val plan = Plan(db)
    val optimizedNone = plan.optimizeNone(query)
    val optimizedBellmanford = plan.optimizeBellmanFord(query)

    val engine1 = EngineSerial(db)
    val engine2 = EngineSerial(db)
    val engineParallel = EngineParallel(db, 30)
    val engine4 = EngineSerial(db)
    val engineRoaringSerial = EngineRoaringSerial(db, 30)
    val engine6 = EngineSerial(db)
    val r1 = engine1.join(optimizedNone)
    val r2 = engine2.join(optimizedBellmanford)
    val r3 = engineParallel.join(optimizedNone)
    val r4 = engineParallel.join(optimizedBellmanford)
    val r5 = engineRoaringSerial.join(optimizedNone)
    val r6 = engineRoaringSerial.join(optimizedBellmanford)

    println("Result1 size: "+r1.size)
    println("Result2 size: "+r2.size)
    println("Result3 size: "+r3.size)
    println("Result4 size: "+r4.size)
    println("Result5 size: "+r5.size)
    println("Result6 size: "+r6.size)
    println("r1==r2 : "+ (r1.size==r2.size).toString)
    println("r1==r3 : "+ (r1.size==r3.size).toString)
    println("r3==r4 : "+ (r3.size==r4.size).toString)
    println("r1==r4 : "+ (r1.size==r4.size).toString)
    println("r1==r5 : "+ (r1.size==r5.size).toString)
    println("r5==r6 : "+ (r5.size==r6.size).toString)
  }


  private def simpleZendo2(): Unit = {
    val params = Params("zendo2")
    val experiment = Experiment(params).load()
    val db = experiment.getDatabase

    val query = Parser.parseHypothesis("zendo(V0):- piece(V0,V1),green(V1),coord1(V1,V3),piece(V0,V2),lhs(V2),coord1(V2,V3).\n"+
      "zendo(V0):- piece(V0,V1),blue(V1),piece(V0,V3),green(V3),piece(V0,V2),red(V2).").get


    val plan = Plan(db)
    val optimizedNone = plan.optimizeNone(query)
    val optimizedBellmanford = plan.optimizeBellmanFord(query)

    val engine1 = EngineSerial(db)
    val engine2 = EngineSerial(db)
    val engineParallel = EngineParallel(db, 30)
    val engine4 = EngineSerial(db)
    val engineRoaringSerial = EngineRoaringSerial(db, 30)
    val engine6 = EngineSerial(db)
    val r1 = engine1.join(optimizedNone)
    val r2 = engine2.join(optimizedBellmanford)
    val r3 = engineParallel.join(optimizedNone)
    val r4 = engineParallel.join(optimizedBellmanford)
    val r5 = engineRoaringSerial.join(optimizedNone)
    val r6 = engineRoaringSerial.join(optimizedBellmanford)

    println("Result1 size: "+r1.size)
    println("Result2 size: "+r2.size)
    println("Result3 size: "+r3.size)
    println("Result4 size: "+r4.size)
    println("Result5 size: "+r5.size)
    println("Result6 size: "+r6.size)
    println("r1==r2 : "+ (r1.size==r2.size).toString)
    println("r1==r3 : "+ (r1.size==r3.size).toString)
    println("r3==r4 : "+ (r3.size==r4.size).toString)
    println("r1==r4 : "+ (r1.size==r4.size).toString)
    println("r1==r5 : "+ (r1.size==r5.size).toString)
    println("r5==r6 : "+ (r5.size==r6.size).toString)
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

    val engineParallel = EngineParallel(db, 5)
    val engineRoaringParallel = EngineRoaringParallel(db, 5)
    val plan = Plan(db)

    val substitution = Substitution().add(Variable("X"), Num("X", 5))


    val r1 = Parser.parseHypothesis("f(X, Y) :- g(X1), X1=X-1, f(X1,Y).").get
    val queries = plan.optimizeExperimental(r1)

    val parallelSubstitutions = engineParallel.join(queries, substitution)
    println("Parallel result: ")
    parallelSubstitutions.foreach(sub=> println(sub))
    println("===========================================")

    val roaringSubstitutions = engineRoaringParallel.join(queries, substitution)
    println("Roaring result: ")
    roaringSubstitutions.foreach(sub=> println(sub))
    println("===========================================")
  }

  def simpleKinship(): Unit = {

    val params = Params("kinship-pi")
    val db = Experiment(params).load().getDatabase

    val engineParallel = EngineParallel(db, 5)
    val engineRoaringParallel = EngineRoaringParallel(db, 5)
    val plan = Plan(db)

    val r1 = Parser.parseHypothesis("inv1(V0,V1):- mother(V0,V1).\n" +
      "inv1(V0,V1):- father(V0,V1).\n"+
      "grandparent(V0,V1):- inv1(V0,V2), inv1(V2,V1).").get
    val queries = plan.optimizeNone(r1)

    val parallelSubstitutions = engineParallel.join(queries, Substitution())
    println("Parallel result: ")
    parallelSubstitutions.foreach(sub=> println(sub))
    println("===========================================")

    val roaringSubstitutions = engineRoaringParallel.join(queries, Substitution())
    println("Roaring result: ")
    roaringSubstitutions.foreach(sub=> println(sub))
    println("===========================================")
  }

  def simpleList(): Unit = {
    val experiment = Experiment(Params("synthesis-next")).load()
    val db = experiment.getDatabase

    val engine = EngineSerial(db, 20)
    val plan = Plan(db)
    val template = BinaryFunctional(engine).setPositives(experiment.getPositives).setNegatives(experiment.getNegatives)

    val hres0 = Parser.parseHypothesis("func3552336(L,T) :- tail(L,T).\n" +
      "func3198432(H,L) :- head(H,L).\n" +
      "func120(A) :- x(A).\n" +
      "func71410313(V0,V1,V2) :- func3198432(V1,V2) & func3552336(V0,V2).\n" +
      "func590646503(V0,V1) :- func120(V3) & func3198432(V3,V0) & func71410313(V0,V1,V2).\n" +
      "func590646503(V0,V1) :- func3552336(V0,V2) & func590646503(V2,V1).").get.buildDependency().compact().buildOperational()

    val hres1 = Parser.parseHypothesis("func3552336(L,T) :- tail(L,T).\n" +
      "func3552336(L,T) :- tail(L,T).\n" +
      "func3198432(H,L) :- head(H,L).\n" +
      "func120(A) :- x(A).\n" +
      "func71410313(V0,V1,V2) :- func3198432(V1,V2) & func3552336(V0,V2).\n" +
      "func590646503(V0,V1) :- func120(V3) & func3198432(V3,V0) & func71410313(V0,V1,V2).\n" +
      "func590646503(V0,V1) :- func3552336(V0,V2) & func590646503(V2,V1).").get.buildDependency().compact().buildOperational()



    val rr1 = Parser.parseRule("func590646503(V0,V1) :- x(V3) & head(V3,V0) & tail(V0,V2) & head(V1,V2).").get
    val rr2 = Parser.parseRule("func590646503(V9,V111) :- tail(V9,V022) & func590646503(V022,V111).").get.buildRecursion()

    val hres2 = Hypothesis(rr1.getHead, Array(rr1, rr2)).build()

    //hres.rules.last.buildRecursion()
    val pp = hres0.normalize().print()
    //func590646503(V0,V1) :- x(V3) & head(V3,V0) & tail(V0,V2) & head(V1,V2).
    //func590646503(V0,G230) :- tail(V0,V2) & x(V3) & head(V3,V2) & tail(V2,V2) & head(G230,V2).
    
    /*val m1 = Parser.parseRule("next_list(V0,V1) :- x(V3) & head(V3,V0) & head(V1,V2) & tail(V0, V2).").get
    val m2 = Parser.parseRule("next_list(V0,V1) :- tail(V0, V2) & next_list(V2, V1).").get.buildRecursion()
    val rr1 = Parser.parseRule("func3552336(V0,V2) :- tail(V0,V2).").get
    val rr2 = Parser.parseRule("func3198432(V1,V2) :- head(V1,V2).").get
    val rr3 = Parser.parseRule("func120(V3) :- x(V3).").get
    val rr4 = Parser.parseRule("func71410313(V0,V1,V2) :- func3198432(V1,V2) & func3552336(V0,V2).").get
    val rr5 = Parser.parseRule("next_list(V0,V1) :- func120(V3) & func3198432(V3,V0) & func71410313(V0,V1,V2).").get
    val rr6 = Parser.parseRule("next_list(V0,V1) :- func3552336(V0,V2) & next_list(V2,V1).").get.buildRecursion()*/

    //val hr1 = Hypothesis(rr5.getHead, Array(rr1, rr2, rr3, rr4, rr5, rr6))
    //  .build()

    val found0 = template.igFunctional(hres2)
    val found1 = template.igFunctional(hres1)

    found0.print()

  }

  def main(args: Array[String]): Unit = {
    simpleWebkb()
  }

}