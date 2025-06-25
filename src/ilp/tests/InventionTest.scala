package ilp.tests

import ilp.data.database.{Engine, Plan}
import ilp.invent.{Execution, HeBinary, HeBinaryFast, HeI, HeII, HeIII, HeUnion}
import ilp.data.{Hypothesis, Parser, Rule, Substitution}
import ilp.experiments.{Experiment, Params}

object InventionTest:

  def testHe(): Unit = {
    val experiment = new Experiment(Params())
    experiment.load()
    val db = experiment.database
    val engine = Engine(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaRule1 = Parser.parseRule("f(X) :- p(X,Y) & s(Y,K).").get
    val metaRule2 = Parser.parseRule("f(X) :- p(X,Y) & s(Y).").get
    val heI = new HeI(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaRule1)

    val heII = new HeII(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaRule2)

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heI)
      .addTemplate(heII).compile()
      .induction()

    results.foreach(h => h.print())

  }

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
  }

  def testIMDB3(): Unit = {
    val experiment = new Experiment(Params("imdb1"))
    experiment.load()

    val db = experiment.database
    val engine = Engine(db)
    val pos = experiment.positives
    val neg = experiment.negatives
    //f(V0,V1):- gender(V0,V3),gender(V1,V3),movie(V2,V0),movie(V2,V1).
    //f(V0,V1):- actor(V0),director(V1),movie(V2,V0),movie(V2,V1).

    val metaTransition1 = Parser.parseRule("movie_director(A,B) :- f(B), g(A,B).").get
    val metaTransition2 = Parser.parseRule("movie_actor(A,B) :- f(B), g(A,B).").get
    val metaTransition3 = Parser.parseRule("tilda(A,B) :- movie_director(Z, B), movie_actor(Z,A).").get
    val q = Parser.parseRule("f(V0,V1):- director(V1),actor(V0),movie(V2,V0),movie(V2,V1).").get
    var h = Hypothesis(q.getHead(), q)

    val t2 = Parser.parseHypothesis("k516(A) :- director(A).\n" +
      "h95(A) :- actor(A).\n"+
      "k721(A,B) :- movie(A,B).\n"+
      "g757(A,B) :- k516(B) & k721(A,B).\n"+
      "c131(A,B) :- h95(B) & k721(A,B).\n"+
      "g634(A,B) :- g757(Z,B) & c131(Z,A).")
/*    val t2 = Parser.parseHypothesis("h95(A1) :- actor(A1).\n" +
      "k721(M1,A3) :- movie(M1,A3).\n" +
      "c131(M2,A4) :- h95(A4) & k721(M2,A4).")*/

    val t1 = Parser.parseHypothesis("f(M, A) :- actor(A), movie(M,A).")
    //val t1 = Parser.parseHypothesis("f(D, A) :- director(D), actor(A), movie(M,D), movie(M,A).")

    val heBinary = new HeBinary(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)

    //val h1 = heBinary.igParallel(t1.get)
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
    val engine = Engine(db)
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
    val engine = Engine(db)
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
    val engine = Engine(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- l(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0) :- g(V0,V4), f(V4, V2).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- w(V0, V1), k(V0, V2).").get
    val metaTransition4 = Parser.parseRule("r4(V0) :- m(V0), n(V0).").get

    val test0 = Parser.parseHypothesis("f(V0):- has_car(V0,V2),roof_open(V2),has_car(V0,V1),roof_closed(V1).\n"+
      "f(V0):- has_car(V0,V1),roof_open(V1),has_load(V1,V2),three_load(V2).\n"+
      "f(V0):- has_car(V0,V4),has_load(V4,V2),rectangle(V2),has_car(V0,V1),has_load(V1,V3),triangle(V3).").get

    val test1 = Parser.parseHypothesis(
      "f1(V0,V2):- has_car(V0,V2), roof_open(V2).\n"+
      "f2(V0, V1) :- has_car(V0,V1), roof_closed(V1).\n"+
      "f(V0) :- f1(V0, V2), f2(V0, V1).\n"+
      "f4(V4, V2) :- has_load(V4, V2), three_load(V2).\n" +
      "f(V0) :- f1(V0, V4), f4(V4, V2).\n" +
      "f5(V1, V3) :- has_load(V1, V3), triangle(V3).\n"+
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

    //val hR = heBinary.igIncremental(test1)
    val results = execution.induction()

    results.foreach(h => h.normalize().print())
  }

  def testHeRecursive(): Unit = {
    val experiment = new Experiment(Params("kinship-ancestor"))
    experiment.load()

    val db = experiment.database
    val engine = Engine(db)
    val pos = experiment.positives
    val neg = experiment.negatives

    val metaRule1 = Parser.parseRule("gamma(A, B) :- alpha(A,Z) & alpha(Z, B).").get
    val metaRule2 = Parser.parseRule("gamma(A, B) :- alpha(A, Z) & mama(Z, B).").get
    val heIII = new HeIII(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaRule1)
      .addMetaRule(metaRule2)


    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heIII)
      .compile()
      .induction()

    results.foreach(h => h.print())
  }


  def main(args: Array[String]): Unit = {
    testTrains3()
  }