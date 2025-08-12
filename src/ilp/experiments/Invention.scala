package ilp.experiments

import ilp.data.database.{Engine, EngineCache, EngineParallel, EngineRoaringSerial, EngineSerial}
import ilp.data.program.{Hypothesis, Parser, Substitution}
import ilp.data.variables.Variable
import ilp.invent.*

object Invention:


  def experiment(): Unit = {
    println("==================================")
    println("Kinship-ancestor")
    //testKinshipAnchestor()
    println("==================================")
    println("imdb3")
    testIMDB3()
    println("==================================")
    println("trains1")
    testTrains1()
    println("================================================")
    println("trains2")
    testTrains2()
    println("================================================")
    println("trains3")
    testTrains3()
    println("================================================")
    println("iggp-attrition-next-score")
    testIGGPAttritionNextScore()
    println("================================================")
    println("iggp-minimal-decay-next-value")
    testIGGPMinimalDecayScore()
    println("================================================")
    println("iggp-gt-chicken-goal")
    testIGGPChickenGoalScore()
    println("================================================")
    println("iggp-sokoban-goal")
    testIGGPSokobanGoalScore()
    println("================================================")
    println("ptc")
    testPTC()
    println("================================================")
    println("pte")
    testPTE()
    println("================================================")
    println("yeast")
    testYeast()
    println("================================================")
    println("uwcs")
    testUWCS()
    println("================================================")
    println("webkb")
    testWebkb()
    println("================================================")
    println("zendo1")
    testZendo1()
    println("================================================")
    println("zendo2")
    testZendo2()

  }
/*
  def testKinshipAnchestor(): Unit = {
    val experiment = new Experiment(Params("kinship-ancestor"))
    experiment.load()

    val db = experiment.database
    val engine = Engine(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaGeneric = Parser.parseRule("gamma(A, B) :- alpha(A,B).").get
    val metaTransition = Parser.parseRule("gamma(A, B) :- alpha(A,Z) & gamma(Z, B).").get

    val heIII = new HeIII(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaGeneric)
      .addMetaRule(metaTransition)


    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heIII)
      .compile()
      .induction()

    results.foreach(h => h.print())
  }*/

  private def testIMDB3(): Unit = {
    val experiment = new Experiment(Params("imdb1"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition1 = Parser.parseRule("r0(A,B) :- f(B), g(A,B).").get
    val metaTransition2 = Parser.parseRule("r1(A,B) :- f(B), g(A,B).").get
    val metaTransition3 = Parser.parseRule("tilda(A,B) :- d(Z, B), a(Z,A).").get
    val q = Parser.parseRule("f(V0,V1):- director(V1),actor(V0),movie(V2,V0),movie(V2,V1).").get
    var h = Hypothesis(q.getHead, q)

    val t2 = Parser.parseHypothesis("k516(A) :- director(A).\n" +
      "h95(A) :- actor(A).\n" +
      "k721(A,B) :- movie(A,B).\n" +
      "g757(A,B) :- k516(B) & k721(A,B).\n" +
      "c131(A,B) :- h95(B) & k721(A,B).\n" +
      "g634(A,B) :- g757(Z,B) & c131(Z,A).")

    val heBinary = new HeBinary(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)

    val h2 = heBinary.igIncremental(t2.get)

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heBinary)
      .compile()
      .induction()

    results.foreach(h => h.print())
  }

  def testTrains1(): Unit = {
    val experiment = new Experiment(Params("trains1-toy"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives


    val metaTransition0 = Parser.parseRule("r0(V0,V1) :- f(V1), g(V0,V1).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- f(V1), g(V0,V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- r0(V0, V1), r1(V0, V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- r1(V0, V2), r2(V0, V1).").get

    val test0 = Parser.parseHypothesis("f(V0) :- has_car(V0,V2),three_wheels(V2),has_car(V0,V1),long(V1),roof_closed(V1).")
    val test1 = Parser.parseHypothesis("k516(A) :- long(A).\n" +
      "a220(A,B) :- has_car(A,B).\n" +
      "e693(A) :- roof_closed(A).\n" +
      "e915(A) :- three_wheels(A).\n" +
      "a458(V0,V1) :- e693(V1) & a220(V0,V1).\n" +
      "k721(V0,V1) :- k516(V1) & a220(V0,V1).\n" +
      "h153(V0,V1) :- e915(V1) & a220(V0,V1).\n" +
      "d537(V0,V1) :- k721(V0,V1) & a458(V0,V1).\n" +
      "c233(V0) :- d537(V0,V2) & h153(V0,V1).").get.compact()

    println(test1.normalize())


    val heBinary = new HeBinary(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .setResembleThreshold(0.9)
      .setResembleWindow(3)

    val v = engine.validHypothesis(test1)
    val hR = heBinary.igIncremental(test1)

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heBinary)
      .compile()
      .induction()

    results.foreach(h => h.print())
  }

  def testTrains2(): Unit = {
    val experiment = new Experiment(Params("trains2"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition0 = Parser.parseRule("r0(V0,V1) :- g(V0,V1), f(V1), z(V1).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- g(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- r0(V0, V1), r1(V0, V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- r1(V0, V2), r2(V0, V1).").get
    val test0 = Parser.parseHypothesis("g(V0):- has_car(V0,V1),roof_open(V1),has_load(V1,V2),triangle(V2).\n" +
      "g(V0):- has_car(V0,V2),two_wheels(V2),roof_open(V2),has_car(V0,V1),roof_closed(V1).").get


    val heBinary = new HeBinaryFast(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .setScoreThreshold(0.9)
      .setResembleThreshold(0.9)
      .setResembleWindow(3)


    val hR = heBinary.igIncremental(test0)

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heBinary)
      .compile()
      .induction()

    results.foreach(h => h.print())
  }

  def testTrains3(): Unit = {
    val experiment = new Experiment(Params("trains3-toy"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- l(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0) :- g(V0,V4), f(V4, V2).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- w(V0, V1), k(V0, V2).").get
    val metaTransition4 = Parser.parseRule("r4(V0) :- m(V0), n(V0).").get

    val test0 = Parser.parseHypothesis("f(V0):- has_car(V0,V2),roof_open(V2),has_car(V0,V1),roof_closed(V1).\n" +
      "f(V0):- has_car(V0,V1),roof_open(V1),has_load(V1,V2),three_load(V2).\n" +
      "f(V0):- has_car(V0,V4),has_load(V4,V2),rectangle(V2),has_car(V0,V1),has_load(V1,V3),triangle(V3).").get

    val test1 = Parser.parseHypothesis(
      "f1(V0,V2):- has_car(V0,V2), roof_open(V2).\n" +
        "f2(V0, V1) :- has_car(V0,V1), roof_closed(V1).\n" +
        "f(V0) :- f1(V0, V2), f2(V0, V1).\n" +
        "f4(V4, V2) :- has_load(V4, V2), three_load(V2).\n" +
        "f(V0) :- f1(V0, V4), f4(V4, V2).\n" +
        "f5(V1, V3) :- has_load(V1, V3), triangle(V3).\n" +
        "f6(V0, V3) :- has_car(V0, V1), f5(V1, V3).\n" +
        "f7(V0, V2) :- has_car(V0, V4), has_load(V4, V2).\n" +
        "f8(V0, V2) :- f7(V0, V2), rectangle(V2).\n" +
        "f(V0) :- f8(V0, V2), f6(V0, V1).").get


    val scoreThreshold = 0.90
    val resembleThreshold = 1.0

    val heBinary = new HeBinary(engine)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .addMetaRule(metaTransition4)
      .setPositiveThreshold(0.05)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnion(engine)
      .setNegativeThreshold(0.05)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(7)
      .setWindow(5)
      .setScoreThreshold(0.9)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testIGGPAttritionNextScore(): Unit = {
    val params = Params("iggp-attrition-next-score")

    val experiment = new Experiment(params)
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- m(V0,V1,V2),l(V4),d(V0,V3,V4),o(V1,V3).").get
    val metaTransition1 = Parser.parseRule("r1(V0, V1, V2) :- m(V0, V1, V2), d(V0,V1,V3), f(V3).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- m(V0,V1, V4), f(V4).").get
    val metaTransition3 = Parser.parseRule("r3(V0, V1, V2) :- w(V2, V3), k(V0, V1, V3).").get
    val metaTransition4 = Parser.parseRule("r4(V0, V1, V2) :- r(V0, V1), n(V0, V1, V2).").get

    val q = Parser.parseHypothesis("next_score(V0,V1,V2):- my_true_score(V0,V1,V2),does(V0,V1,V5),beats(V3,V5),does(V0,V4,V3).\n" +
      "next_score(V0,V1,V2):- my_true_score(V0,V1,V2),does(V0,V1,V3),different(V1,V4),does(V0,V4,V3).\n" +
      "next_score(V0,V1,V2):- my_true_score(V0,V1,V5),my_succ(V5,V2),does(V0,V1,V6),beats(V6,V3),does(V0,V4,V3).").get

    val scoreThreshold = 0.90
    val resembleThreshold = 1.0

    val heBinary = new HeBinary(engine)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .addMetaRule(metaTransition4)
      .setPositiveThreshold(0.05)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnion(engine)
      .setNegativeThreshold(0.09)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(7)
      .setWindow(5)
      .setScoreThreshold(0.9)
      .setNegThreshold(0.07)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    heBinary.igCache(q).print()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testIGGPMinimalDecayScore(): Unit = {
    val experiment = new Experiment(Params("iggp-minimal-decay-next-value"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- d(V0,V4,V2),l(V2).").get
    val metaTransition1 = Parser.parseRule("r1(V0, V1) :- m(V0, V4, V2), f(V1).").get

    val metaTransition2 = Parser.parseRule("r1(V0, V1, V2) :- w(V1, V2), k(V0, V2).").get
    val metaTransition3 = Parser.parseRule("r4(V0, V1) :- r(V0, V1, V2), n(V0, V4, V3).").get

    val q = Parser.parseHypothesis("next_value(V0,V1):- c5(V1),does(V0,V3,V2),press_button(V2).\n" +
      "next_value(V0,V1):- my_succ(V1,V2),true_value(V0,V2),does(V0,V4,V3),noop(V3).").get

    val scoreThreshold = 0.9
    val resembleThreshold = 1.0

    val heBinary = new HeBinary(engine)
      .addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .setPositiveThreshold(0.01)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnion(engine)
      .setNegativeThreshold(0.01)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(7)
      .setWindow(10)
      .setScoreThreshold(scoreThreshold)
      .setNegThreshold(0.01)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    heBinary.igCache(q).print()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testIGGPChickenGoalScore(): Unit = {
    val experiment = new Experiment(Params("iggp-gt-chicken-goal"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- m(V0,V2),l(V1).").get

    val q = Parser.parseHypothesis("goal(V0,V1,V2):- true_whiteScore(V0,V2),agent_white(V1).\n" +
      "goal(V0,V1,V2):- true_blackScore(V0,V2),agent_black(V1).").get

    val scoreThreshold = 0.90
    val resembleThreshold = 1.0

    val heBinary = new HeBinary(engine)
      .addMetaRule(metaTransition0)
      .setPositiveThreshold(0.01)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnion(engine)
      .setNegativeThreshold(0.09)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(7)
      .setWindow(5)
      .setScoreThreshold(0.9)
      .setNegThreshold(0.01)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    heBinary.igCache(q).print()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testIGGPSokobanGoalScore(): Unit = {
    val experiment = new Experiment(Params("iggp-sokoban-goal"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition0 = Parser.parseRule("r1(V0,V4,V5,V6) :- t(V0,V4,V6,V5),l(V5).").get
    val metaTransition1 = Parser.parseRule("r2(V0,V3,V4,V5,V6) :- f1(V0,V6,V4,V3),f2(V0,V6,V4,V5).").get
    val metaTransition2 = Parser.parseRule("r3(V0,V1,V2) :- r4(V0,V6,V4), r5(V0,V3,V4,V5,V6), rk(V1), rn(V2).").get

    val scoreThreshold = 0.90
    val resembleThreshold = 1.0

    val heBinary = new HeBinary(engine)
      .addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .setPositiveThreshold(0.01)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnion(engine)
      .setNegativeThreshold(0.09)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(7)
      .setWindow(5)
      .setFilterSize(5)
      .setScoreThreshold(0.9)
      .setNegThreshold(0.01)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testPTC(): Unit = {

    /*
    label(V0):- zn(V2),atom(V1,V0,V2).
    label(V0):- cu(V2),atom(V1,V0,V2).
    label(V0):- c(V4),connected(V3,V5,V2),atom(V5,V0,V4),p(V1),atom(V3,V0,V1).
    label(V0):- connected(V3,V5,V2),atom(V5,V0,V4),p(V1),h(V4),atom(V3,V0,V1).
     */

    val experiment = new Experiment(Params("ptc"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition0 = Parser.parseRule("r1(V0,V1,V2) :- t(V2),l(V1,V0,V2).").get
    val metaTransition1 = Parser.parseRule("r2(V0) :- r(V3,V5,V2), k(V5,V0,V4), j(V3,V0,V1).").get
    val metaTransition2 = Parser.parseRule("r3(V0) :- r1(V0,V1,V2).").get


    val scoreThreshold = 0.90
    val resembleThreshold = 1.0

    val heBinary = new HeBinaryFast(engine)
      .addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .setPositiveThreshold(0.01)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.05)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(7)
      .setWindow(5)
      .setFilterSize(100)
      .setScoreThreshold(0.9)
      .setNegThreshold(0.01)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  private def testPTE(): Unit = {

    /*
    label(V0):- zn(V2),atom(V1,V0,V2).
    label(V0):- cu(V2),atom(V1,V0,V2).
    label(V0):- c(V4),connected(V3,V5,V2),atom(V5,V0,V4),p(V1),atom(V3,V0,V1).
    label(V0):- connected(V3,V5,V2),atom(V5,V0,V4),p(V1),h(V4),atom(V3,V0,V1).
     */

    val experiment = new Experiment(Params("pte"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    //val metaTransition0 = Parser.parseRule("r(V0) :- k(V1,V3), m(V1), x(V1,V3), a(V0,V3,V4,V2,V5).").get
    val metaTransition1 = Parser.parseRule("r(V0) :- o(V5,V1), k(V5,V1), a(V0,V1,V2,V4,V3).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- i(V5,V1), s(V5), a(V0,V1,V2,V4,V3).").get

    val scoreThreshold = 0.90
    val resembleThreshold = 1.0

    val heBinary = new HeBinaryFast(engine)
      //.addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .setPositiveThreshold(1.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.0005)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(4)
      .setWindow(3)
      .setFilterSize(5)
      .setScoreThreshold(0.9)
      .setNegThreshold(0.01)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testYeast(): Unit = {

    /*
    proteins(V0):- path(V0,V2),location(V0,V1).
    proteins(V0):- enzyme(V0,V1),renzyme(V0,V1).
    proteins(V0):- path(V2,V3),interaction(V3,V0,V1).
    proteins(V0):- protein_class(V0,V1),rprotein_class(V0,V1).
    proteins(V0):- protein_class(V0,V4),interaction(V2,V0,V1),rprotein_class(V3,V4).
    proteins(V0):- phenotype(V0,V3),renzyme(V0,V2),rphenotype(V1,V3).
    proteins(V0):- protein_class(V0,V3),rprotein_class(V2,V3),enzyme(V2,V1).
    proteins(V0):- interaction(V3,V0,V1),protein_class(V3,V2),rprotein_class(V3,V2).
    proteins(V0):- path(V2,V1),interaction(V2,V0,V3),rprotein_class(V0,V4).
     */

    val experiment = new Experiment(Params("yeast"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    //val metaTransition0 = Parser.parseRule("r(V0) :- k(V1,V3), m(V1), x(V1,V3), a(V0,V3,V4,V2,V5).").get
    val metaTransition1 = Parser.parseRule("r(V0) :- p(V0, V2), k(V0,V1).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- p(V0, V1), k(V0,V1).").get
    val metaTransition3 = Parser.parseRule("r(V0) :- p(V0, V3), k(V0,V2), e(V2,V1).").get
    val metaTransition4 = Parser.parseRule("r(V0) :- i(V3,V0,V1), p(V3,V2), r(V3,V2).").get
    val metaTransition5 = Parser.parseRule("r(V0) :- p(V2,V1), i(V2,V0,V3), r(V0,V4).").get

    val scoreThreshold = 0.90
    val resembleThreshold = 1.0
    val filterSize = 100

    val heBinary = new HeBinaryFast(engine)
      //.addMetaRule(metaTransition0)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .addMetaRule(metaTransition4)
      .addMetaRule(metaTransition5)
      .setPositiveThreshold(1.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.0005)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(4)
      .setWindow(3)
      .setFilterSize(filterSize)
      .setScoreThreshold(0.9)
      .setNegThreshold(0.01)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }


  def testUWCS(): Unit = {

    /*

    advisedBy(V0,V1):- student(V0),tempAdvisedBy(V3,V1),taughtBy(V4,V0,V5),taughtBy(V4,V1,V2).
    advisedBy(V0,V1):- ta(V5,V0,V3),taughtBy(V5,V1,V3),tempAdvisedBy(V4,V1),publication(V2,V0),publication(V2,V1),publication(V2,V4).

     */

    val experiment = new Experiment(Params("uwcs"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives


    val metaTransition1 = Parser.parseRule("r(V0, V1, V2, V4) :- a(V4, V0, V5), a(V4, V1, V2).").get
    val metaTransition2 = Parser.parseRule("r(V0, V1, V2, V3, V4) :- t(V0, V1, V2, V4), a(V3, V1).").get
    val metaTransition3 = Parser.parseRule("r(V0, V1) :- t(V0, V1, V2, V3, V4), s(V0).").get

    val metaTransition4 = Parser.parseRule("r(V0, V1, V2, V4) :- p(V2, V0), p(V2, V1), p(V2, V4).").get
    val metaTransition5 = Parser.parseRule("r(V0, V1, V2, V4) :- t(V4, V1), k(V0, V1, V2, V4).").get
    val metaTransition6 = Parser.parseRule("r(V0, V1) :- t(V5, V0, V3), a(V5, V1, V3), k(V0, V1, V2, V4).").get


    val scoreThreshold = 0.14
    val resembleThreshold = 1.0
    val filterSize = 1000

    val heBinary = new HeBinaryFast(engine)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .addMetaRule(metaTransition4)
      .addMetaRule(metaTransition5)
      .addMetaRule(metaTransition6)
      .setPositiveThreshold(1.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.0)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(10)
      .setWindow(50)
      .setFilterSize(filterSize)
      .setScoreThreshold(scoreThreshold)
      .setNegThreshold(1.0)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testWebkb(): Unit = {

    val experiment = new Experiment(Params("webkb"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives


    val metaTransition1 = Parser.parseRule("r(V0, V4) :- co(V1, V0), pr(V4, V0).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- t(V0, V4), a(V3, V4).").get
    val scoreThreshold = 0.14
    val resembleThreshold = 1.0
    val filterSize = 1000

    val heBinary = new HeBinaryFast(engine)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .setPositiveThreshold(1.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.0)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(10)
      .setWindow(50)
      .setFilterSize(filterSize)
      .setScoreThreshold(scoreThreshold)
      .setNegThreshold(1.0)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testZendo1(): Unit = {

    val experiment = new Experiment(Params("zendo1"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives


    val metaTransition1 = Parser.parseRule("r(V0, V1, V2) :- co(V0, V2), pr(V2, V1).").get
    val metaTransition2 = Parser.parseRule("r(V2, V3) :- t(V2, V3), a(V3).").get
    val metaTransition3 = Parser.parseRule("r(V0, V1, V2) :- r1(V0, V1, V2), a(V1).").get
    val metaTransition4 = Parser.parseRule("r(V0) :- r1(V0, V1, V2), r2(V2, V3).").get

    val scoreThreshold = 0.997
    val resembleThreshold = 1.0
    val filterSize = 1000

    val heBinary = new HeBinaryFast(engine)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .addMetaRule(metaTransition4)
      .setPositiveThreshold(1.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.0)
      .setPositiveThreshold(0.01)
      .setScoreThreshold(scoreThreshold)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(10)
      .setWindow(50)
      .setFilterSize(filterSize)
      .setScoreThreshold(scoreThreshold)
      .setNegThreshold(1.0)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }


  def testZendo2(): Unit = {

    val experiment = new Experiment(Params("zendo2"))
    experiment.load()

    val db = experiment.database
    val engine = EngineSerial(db)
    val pos = experiment.positives
    val neg = experiment.negatives
    val windowSize = 3

    val metaTransition1 = Parser.parseRule("re(V0, V1) :- pi(V0, V1), s(V1).").get
    val metaTransition4 = Parser.parseRule("re(V1, V2, V3) :- c(V1, V3), c(V2, V3).").get
    val metaTransition2 = Parser.parseRule("re(V0) :- r1(V0, V1), r2(V0, V2), r3(V0, V3).").get
    val metaTransition5 = Parser.parseRule("re(V0) :- r1(V0, V1), r2(V0, V2), r3(V1, V2, V3).").get


    val scoreThreshold = 0.94
    val resembleThreshold = 1.0
    val filterSize = 200

    val heBinary = new HeBinaryFast(engine)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition4)
      .addMetaRule(metaTransition5)
      .setPositiveThreshold(0.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setFilterSize(filterSize)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFast(engine)
      .setNegativeThreshold(0.0)
      .setPositiveThreshold(0.05)
      .setScoreThreshold(scoreThreshold)
      .setFilterSize(filterSize)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(10)
      .setWindow(windowSize)
      .setFilterSize(filterSize)
      .setScoreThreshold(scoreThreshold)
      .setNegThreshold(1.0)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  private def testSynthesis(): Unit = {

    val experiment = new Experiment(Params("synthesis-next"))
    experiment.load()

    val db = experiment.database
    val engine:Engine = EngineParallel(db, 7)
    val pos = experiment.positives
    val neg = experiment.negatives
    val windowSize = 3

    val metaRecursion = Parser.parseRule("re(V0, V1) :- pi(V0, V2), re(V2,V1).").get
      .buildRecursion()
    val metaRest1 = Parser.parseRule("re(V0, V1, V2) :- t(V0, V2), c(V1, V2).").get
    val metaRest2 = Parser.parseRule("re(V0, V1) :- rest(V0, V1, V2), r2(V3, V0), alpha(V3).").get

    val p1 = Parser.parseRule("next_list(V0,V1):- tail(V0,V2),head(V1,V2), head(V3,V0), x(V3).").get
    val p2 = Parser.parseRule("next_list(V0,V1):- tail(V0,V2),next_list(V2,V1).").get
      .buildRecursion()

    val hypothesis = Hypothesis(Array(p1, p2)).build()

    val scoreThreshold = 0.94
    val resembleThreshold = 1.0
    val filterSize = 100

    val heBinary = new HeBinaryFunctional(engine)
      .addMetaRule(metaRecursion)
      .addMetaRule(metaRest1)
      .addMetaRule(metaRest2)
      .setPositiveThreshold(0.0)
      .setNegativeThreshold(1.0)
      .setScoreThreshold(scoreThreshold)
      .setFilterSize(filterSize)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val heUnion = new HeUnionFunctional(engine)
      .setNegativeThreshold(0.0)
      .setPositiveThreshold(0.05)
      .setScoreThreshold(scoreThreshold)
      .setFilterSize(filterSize)
      .setResembleThreshold(resembleThreshold)
      .setResembleWindow(3)

    val execution = Execution(engine)
      .setIter(10)
      .setWindow(windowSize)
      .setFilterSize(filterSize)
      .setScoreThreshold(scoreThreshold)
      .setNegThreshold(1.0)
      .setPositives(pos)
      .setNegatives(neg)
      .addTemplate(heUnion)
      .addTemplate(heBinary)
      .compile()

    heBinary.igFunctional(hypothesis).print()

    val results = execution.induction()
    val best = results.toArray.minBy(h=> h.getRuleSize)
    best.print()
  }

  def main(args: Array[String]): Unit = {
    testSynthesis()

  }