package ilp.experiments

import ilp.data.database.{EngineCache, EngineParallel}
import ilp.data.program.{Hypothesis, Parser, Rule, Substitution}
import ilp.data.variables.Variable
import ilp.experiments.Invention.experiment
import ilp.invent.*

import java.io.PrintWriter
import scala.Tuple.Union

object Invention:


  def experiment(): Unit = {
    var results = Array[String]()
    //results ++= testKinshipPi()
    //results ++= testIMDB1()
    //results ++= testTrains1()
    //results ++= testTrains2()
    //results ++= testTrains3()
    //results ++= testIGGPAttritionNextScore() //error
    //results ++= testIGGPMinimalDecayScore()
    //results ++= testIGGPChickenGoalScore()
    //results ++= testIGGPSokobanGoalScore()
    results ++= testPTC()
    results ++= testPTE()
    results ++= testYeast()
    results ++= testUWCS()
    results ++= testWebkb()
    results ++= testZendo1()
    results ++= testZendo2()
    results ++= testSynthesis()

    val pw = new PrintWriter("resources/experiments/invention.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line=> pw.println(line))
    pw.close()

  }

  def measureTime[T](block: => T): (T, Double) = {

    val time = {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      val elapsedTime = (end - start) / 1e6
      (result, elapsedTime)
    }

     time
  }

  def measureResult(experiment: Experiment, metaRules:Array[Template]):String =
    val params = experiment.getParams
    val db = experiment.database
    val engine = EngineParallel(db, params.recursionSize)
    val pos = experiment.positives
    val neg = experiment.negatives


    val execution = Execution(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .setMaxRules(params.maxRules)
      .setIter(params.iterationsSize)
      .setWindow(params.windowSize)
      .setFilterSize(params.filterSize)

    metaRules.foreach(metaTemplate=> execution.addTemplate(metaTemplate))
    execution.compile()

    val (results, time) = measureTime(execution.induction())
    val maxScore = results.toArray.map(hypothesis => hypothesis.score)
      .maxOption.getOrElse(0d)

    val line = params.toLine(time, maxScore)
    line

  def testKinshipPi(): Array[String] = {

    val metaBase = Parser.parseRule("parent(A, B) :- alpha(A,B).").get
    val metaTransition = Parser.parseRule("grandparent(A, B) :- parent(A,Z), parent(Z, B).").get
      .buildRecursion()

    val params = Params("kinship-pi").generateParams().take(5)
    params.map(param=>{
      val experiment = Experiment(param).load()
      val engine = EngineParallel(experiment.getDatabase, param.recursionSize)
      val heRecursive = new Binary(engine)
        .setScoreThreshold(param.scoreThreshold)
        .setResembleThreshold(param.resembleThreshold)
        .setResembleWindow(param.resembleWindow)
        .setPositiveThreshold(param.binaryPositiveThreshold)
        .setNegativeThreshold(param.binaryNegativeThreshold)
        .addMetaRule(metaBase)
        .addMetaRule(metaTransition)

      val heUnion = new UnionBinary(engine)
        .setPositiveThreshold(param.unionPositiveThreshold)
        .setNegativeThreshold(param.unionNegativeThreshold)
        .setResembleThreshold(param.resembleThreshold)
        .setResembleWindow(param.resembleWindow)
        .setScoreThreshold(param.scoreThreshold)


      measureResult(experiment, Array(heRecursive, heUnion))
    })
  }

  private def testIMDB1(): Array[String] = {

    val metaTransition1 = Parser.parseRule("r0(A,B) :- f(B), g(A,B).").get
    val metaTransition2 = Parser.parseRule("r1(A,B) :- f(B), g(A,B).").get
    val metaTransition3 = Parser.parseRule("tilda(A,B) :- d(Z, B), a(Z,A).").get
    val parameters = Params("imdb1").generateParams()

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)
      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setPositiveThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleThreshold(params.resembleWindow)

      measureResult(experiment, Array(heBinary))
    })

  }

  def testTrains1(): Array[String] = {
    val parameters = Params("trains1-toy").generateParams()
    val metaTransition0 = Parser.parseRule("r0(V0,V1) :- f(V1), g(V0,V1).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- f(V1), g(V0,V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- r0(V0, V1), r1(V0, V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- r1(V0, V2), r2(V0, V1).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setResembleThreshold(0.9)
        .setResembleWindow(3)

      measureResult(experiment, Array(heBinary))
    })


  }

  def testTrains2(): Array[String] = {
    val parameters = Params("trains2").generateParams()

    val metaTransition0 = Parser.parseRule("r0(V0,V1) :- g(V0,V1), f(V1), z(V1).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- g(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- r0(V0, V1), r1(V0, V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- r1(V0, V2), r2(V0, V1).").get
    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary))
    })
  }

  def testTrains3(): Array[String] = {
    val parameters = Params("trains3-toy").generateParams()
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- l(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0) :- g(V0,V4), f(V4, V2).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- w(V0, V1), k(V0, V2).").get
    val metaTransition4 = Parser.parseRule("r4(V0) :- m(V0), n(V0).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testIGGPAttritionNextScore(): Array[String] = {
    val parameters = Params("iggp-attrition-next-score").generateParams()
    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- m(V0,V1,V2),l(V4),d(V0,V3,V4),o(V1,V3).").get
    val metaTransition1 = Parser.parseRule("r1(V0, V1, V2) :- m(V0, V1, V2), d(V0,V1,V3), f(V3).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- m(V0,V1, V4), f(V4).").get
    val metaTransition3 = Parser.parseRule("r3(V0, V1, V2) :- w(V2, V3), k(V0, V1, V3).").get
    val metaTransition4 = Parser.parseRule("r4(V0, V1, V2) :- r(V0, V1), n(V0, V1, V2).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionBinary(engine)

        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testIGGPMinimalDecayScore(): Array[String] = {
    val parameters = Params("iggp-minimal-decay-next-value").generateParams()
    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- d(V0,V4,V2),l(V2).").get
    val metaTransition1 = Parser.parseRule("r1(V0, V1) :- m(V0, V4, V2), f(V1).").get
    val metaTransition2 = Parser.parseRule("r1(V0, V1, V2) :- w(V1, V2), k(V0, V2).").get
    val metaTransition3 = Parser.parseRule("r4(V0, V1) :- r(V0, V1, V2), n(V0, V4, V3).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(0.01)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }


  def testIGGPChickenGoalScore(): Array[String] = {
    val parameters = Params("iggp-gt-chicken-goal-tiny").generateParams()
    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- m(V0,V2),l(V1).").get

    parameters.map(params=>{
      //Condition
      params.maxRules = 7
      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition0)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testIGGPSokobanGoalScore(): Array[String] = {
    val parameters = Params("iggp-sokoban-goal").generateParams()

    val metaTransition0 = Parser.parseRule("r0(V0,V4,V5,V6) :- t(V0,V4,V6,V5),l(V5).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V3,V4,V5,V6) :- f1(V0,V6,V4,V3),f2(V0,V6,V4,V5).").get
    val metaTransition2 = Parser.parseRule("r2(V0,V1,V2,V3) :- t1(V0,V1,V2), a1(V0,V1,V2,V3).").get
    val metaTransition3 = Parser.parseRule("r3(V0,V1,V2) :- r4(V0,V6,V4), r5(V0,V3,V4,V5,V6), rk(V1), rn(V2).").get

    parameters.map(params=>{
      params.maxRules = 50
      params.iterationsSize = 20
      params.filterSize = 1000
      val experiment = new Experiment(params).load()

      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setPositiveThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }


  def testPTC(): Array[String] = {
    val parameters = Params("ptc").generateParams()
    /*
    * label(V0):- zn(V2),atom(V1,V0,V2).
      label(V0):- cu(V2),atom(V1,V0,V2).
      label(V0):- c(V4),connected(V3,V5,V2),atom(V5,V0,V4),p(V1),atom(V3,V0,V1).
      label(V0):- connected(V3,V5,V2),atom(V5,V0,V4),p(V1),h(V4),atom(V3,V0,V1).*/
    val metaTransition0 = Parser.parseRule("r1(V0) :- t(V2),l(V1,V0,V2).").get
    val metaTransition1 = Parser.parseRule("r2(V0, V1, V2) :- t(V2),l(V1,V0,V2).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1, V2, V3, V4) :- l(V3,V5,V2), m(V5,V0,V4), r(V3,V0,V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- m(V0,V1,V2,V3,V4,V5), c(V1), r(V4).").get

    parameters.map(params=>{
      params.filterSize = 2000
      params.unionPositiveThreshold = 0.0005
      params.unionNegativeThreshold = 0.0
      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testPTE(): Array[String] = {
    val parameters = Params("pte").generateParams()
    val metaTransition0 = Parser.parseRule("r(V0) :- k(V1,V3), m(V1), x(V1,V3), a(V0,V3,V4,V2,V5).").get
    val metaTransition1 = Parser.parseRule("r(V0) :- o(V5,V1), k(V5,V1), a(V0,V1,V2,V4,V3).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- i(V5,V1), s(V5), a(V0,V1,V2,V4,V3).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testYeast(): Array[String] = {
    val parameters = Params("yeast").generateParams()
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
    //val metaTransition0 = Parser.parseRule("r(V0) :- k(V1,V3), m(V1), x(V1,V3), a(V0,V3,V4,V2,V5).").get
    val metaTransition1 = Parser.parseRule("r(V0) :- p(V0, V2), k(V0,V1).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- p(V0, V1), k(V0,V1).").get
    val metaTransition3 = Parser.parseRule("r(V0) :- p(V0, V3), k(V0,V2), e(V2,V1).").get
    val metaTransition4 = Parser.parseRule("r(V0) :- i(V3,V0,V1), p(V3,V2), r(V3,V2).").get
    val metaTransition5 = Parser.parseRule("r(V0) :- p(V2,V1), i(V2,V0,V3), r(V0,V4).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .addMetaRule(metaTransition5)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testUWCS(): Array[String] = {
    val parameters = Params("uwcs").generateParams()
    val metaTransition1 = Parser.parseRule("r(V0, V1, V2, V4) :- a(V4, V0, V5), a(V4, V1, V2).").get
    val metaTransition2 = Parser.parseRule("r(V0, V1, V2, V3, V4) :- t(V0, V1, V2, V4), a(V3, V1).").get
    val metaTransition3 = Parser.parseRule("r(V0, V1) :- t(V0, V1, V2, V3, V4), s(V0).").get

    val metaTransition4 = Parser.parseRule("r(V0, V1, V2, V4) :- p(V2, V0), p(V2, V1), p(V2, V4).").get
    val metaTransition5 = Parser.parseRule("r(V0, V1, V2, V4) :- t(V4, V1), k(V0, V1, V2, V4).").get
    val metaTransition6 = Parser.parseRule("r(V0, V1) :- t(V5, V0, V3), a(V5, V1, V3), k(V0, V1, V2, V4).").get
    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .addMetaRule(metaTransition5)
        .addMetaRule(metaTransition6)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testWebkb(): Array[String] = {
    val parameters = Params("webkb").generateParams()

    val metaTransition1 = Parser.parseRule("r(V0, V4) :- co(V1, V0), pr(V4, V0).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- t(V0, V4), a(V3, V4).").get
    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testZendo1(): Array[String] = {
    val parameters = Params("zendo1").generateParams()
    val metaTransition1 = Parser.parseRule("r(V0, V1, V2) :- co(V0, V2), pr(V2, V1).").get
    val metaTransition2 = Parser.parseRule("r(V2, V3) :- t(V2, V3), a(V3).").get
    val metaTransition3 = Parser.parseRule("r(V0, V1, V2) :- r1(V0, V1, V2), a(V1).").get
    val metaTransition4 = Parser.parseRule("r(V0) :- r1(V0, V1, V2), r2(V2, V3).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testZendo2(): Array[String] = {
    val parameters = Params("zendo2").generateParams()
    val metaTransition1 = Parser.parseRule("re(V0, V1) :- pi(V0, V1), s(V1).").get
    val metaTransition4 = Parser.parseRule("re(V1, V2, V3) :- c(V1, V3), c(V2, V3).").get
    val metaTransition2 = Parser.parseRule("re(V0) :- r1(V0, V1), r2(V0, V2), r3(V0, V3).").get
    val metaTransition5 = Parser.parseRule("re(V0) :- r1(V0, V1), r2(V0, V2), r3(V1, V2, V3).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition5)
        .addMetaRule(metaTransition4)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def testSynthesis(): Array[String] = {
    val parameters = Params("synthesis-next").generateParams()
    val metaRecursion = Parser.parseRule("re(V0, V1) :- pi(V0, V2), re(V2,V1).").get
      .buildRecursion()
    val metaRest1 = Parser.parseRule("re(V0, V1, V2) :- t(V0, V2), c(V1, V2).").get
    val metaRest2 = Parser.parseRule("re(V0, V1) :- rest(V0, V1, V2), r2(V3, V0), alpha(V3).").get

    parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFunctional(engine)
        .addMetaRule(metaRecursion)
        .addMetaRule(metaRest1)
        .addMetaRule(metaRest2)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFunctional(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })
  }

  def main(args: Array[String]): Unit = {
    experiment()
  }