package ilp.tests

import ilp.data.database.{Engine, Plan}
import ilp.invent.{Execution, HeBinary, HeI, HeII, HeIII}
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
      .compile()
    val heII = new HeII(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaRule2)
      .compile()

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heI)
      .addTemplate(heII).induction()

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
      .compile()

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heIII).induction()

    results.foreach(h => h.print())
  }

  def testIMDB3(): Unit = {
    val experiment = new Experiment(Params("imdb3"))
    experiment.load()

    val db = experiment.database
    val engine = Engine(db)
    val pos = experiment.positives
    val neg = experiment.negatives
    //f(V0,V1):- gender(V0,V3),gender(V1,V3),movie(V2,V0),movie(V2,V1).
    //f(V0,V1):- actor(V0),director(V1),movie(V2,V0),movie(V2,V1).
    val metaTransition1 = Parser.parseRule("theta(V0,V1) :- alpha(V0,V3), alpha(V1,V3).").get
    val metaTransition2 = Parser.parseRule("beta(V0,V1) :- gamma(V2, V0), gamma(V2,V1).").get
    val metaTransition3 = Parser.parseRule("tilda(V0,V1) :- theta(V0, V1), beta(V0,V1).").get

    val q = Parser.parseRule("f(V0,V1):- gender(V0,V3),gender(V1,V3),movie(V2,V0),movie(V2,V1).").get
    val optimized = Plan(db).optimizeExperimental(Hypothesis(q))
    val facts = engine.joinCyclicRoaring(optimized, Substitution())

    val heIV = new HeBinary(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .addMetaRule(metaTransition1)
      .addMetaRule(metaTransition2)
      .addMetaRule(metaTransition3)
      .compile()

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heIV).induction()

    results.foreach(h => h.print())
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
      .compile()

    val results = Execution(engine)
      .setIter(5)
      .addTemplate(heIII).induction()

    results.foreach(h => h.print())
  }


  def main(args: Array[String]): Unit = {
    testIMDB3()
  }