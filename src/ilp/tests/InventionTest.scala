package ilp.tests

import ilp.data.database.Engine
import ilp.invent.{Execution, HeI, HeII, HeIII}
import ilp.data.{Hypothesis, Parser, Rule}
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

    val results = Execution()
      .setIter(5)
      .addTemplate(heI)
      .addTemplate(heII).induction()

    results.foreach(h=> h.print())

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

    val results = Execution()
      .setIter(5)
      .addTemplate(heIII).induction()

    results.foreach(h=> h.print())

  }

/*
  def testRecursiveRule(): Unit = {
    val r1 = Parser.parseRule("ancestor(X, Y) :- mother(X,Y).").get
    val r2 = Parser.parseRule("ancestor(X, Y) :- father(X,Y).").get
    val r3 = Parser.parseRule("ancestor(X, Z) :- ancestor(X,Y) & ancestor(Y,Z).").get
    val h = Hypothesis(Set[Rule](r1,r2,r3))
    val experiment = new Experiment(Params("kinship-ancestor")).load()
    val db = experiment.database
    val pos = experiment.positives
    val neg = experiment.negatives
    val facts = db.facts(h)
    h.ig(facts, pos, neg)
    h.accuracy()
    h.print()
  }
*/

  def main(args: Array[String]): Unit = {
    testHeRecursive()
  }