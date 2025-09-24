package ilp.experiments

import ilp.data.database.{EngineCache, EngineParallel, EngineSerial}
import ilp.data.program.{Hypothesis, Parser, Rule, Substitution}
import ilp.invent.*

import java.io.PrintWriter


object Invention:


  def experiment(): Unit = {
    var results = Array[String]()
    //results ++= testKinshipPi()
    //results ++= testIMDB1()
    //results ++= testIMDB3()
    //results ++= testTrains1()
    //results ++= testTrains2()
    //results ++= testTrains3()
    //results ++= testIGGPAttritionNextScore()
    //results ++= testIGGPMinimalDecayScore()
    //results ++= testIGGPChickenGoalScore()
    //results ++= testIGGPSokobanGoalScore()
    results ++= Invention.testSynthesisContains()
    //results ++= testPTC()
    //results ++= testPTE()
    //results ++= testYeast() //semi-success
    //results ++= testUWCS()
    //results ++= testWebkb() //semi-success
    //results ++= testZendo1()
    //results ++= testZendo2()
    //results ++= testSynthesis()

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
    val engine = EngineCache(db, params.recursionSize)
    val pos = experiment.positives
    val neg = experiment.negatives

    val execution = Execution(engine)
      .setPositives(pos)
      .setNegatives(neg)
      .setMaxRules(params.maxRules)
      .setIter(params.iterationsSize)
      .setWindow(params.windowSize)
      .setFilterSize(params.filterSize)
      .setUntestedSize(params.unTestedSize)
      .setScoreThreshold(params.scoreThreshold)

    metaRules.foreach(metaTemplate=> execution.addTemplate(metaTemplate))
    execution.compile()

    val (results, time) = measureTime(execution.induction())
    val maxScore = results.toArray.map(hypothesis => hypothesis.score)
      .maxOption.getOrElse(0d)

    val line = params.toLine(results.size, time, maxScore)
    //println(line)
    val found = results.toArray.sortBy(_.score).reverse.head
    val rule = found.normalize()
    rule.print()
    found.print()
    line

  def testKinshipPi(): Array[String] = {

    val metaBase = Parser.parseRule("parent(A, B) :- alpha(A,B).").get
    val metaTransition = Parser.parseRule("grandparent(A, B) :- parent(A,Z), parent(Z, B).").get
      .buildRecursion()

    val params = Params("kinship-pi").generateParams()
    val results = params.map(param=>{
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

    val pw = new PrintWriter("resources/experiments/inventions/kinship-pi.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  private def testIMDB1(): Array[String] = {

    val metaTransition1 = Parser.parseRule("r0(A,B) :- f(B), g(A,B).").get
    val metaTransition2 = Parser.parseRule("r1(A,B) :- f(B), g(A,B).").get
    val metaTransition3 = Parser.parseRule("tilda(A,B) :- d(Z, B), a(Z,A).").get
    val parameters = Params("imdb1").generateParams()

    val results = parameters.map(params=>{
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

    val pw = new PrintWriter("resources/experiments/inventions/imdb1.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  private def testIMDB3(): Array[String] = {

    val metaTransitionB0 = Parser.parseRule("r0(V0,V1,V3) :- g(V0,V3), g(V1, V3).").get
    val metaTransitionB1 = Parser.parseRule("r0(V0,V1) :- f(V0,V1,V3), p(V2, V0), p(V2, V1).").get

    val metaTransitionL0 = Parser.parseRule("r0(A,B) :- f(B), g(A,B).").get
    val metaTransitionL1 = Parser.parseRule("r0(A,B) :- d(Z, B), a(Z,A).").get

    val parameters = Params("imdb3").generateParams()
    val hypothesis = Parser.parseHypothesis("f(E870,L387) :- movie(K910,E870) & movie(K910,L387) & gender(E870,L902) & gender(L387,L902).")
    //func294359557(C413,J547) :- gender(C57,C413) & gender(C57,J547) & movie(C413,D141) & movie(J547,D141).
    val results = parameters.map(params=>{

      params.scoreThreshold = 1.0
      params.binaryPositiveThreshold = 0.0
      params.binaryNegativeThreshold = 1.0

      params.filterSize = 10000
      params.windowSize = 5
      params.iterationsSize = 5

      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)
      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransitionB0)
        .addMetaRule(metaTransitionB1)
        .addMetaRule(metaTransitionL0)
        .addMetaRule(metaTransitionL1)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleThreshold(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      val heUnion = new UnionFast(engine)
        .setScoreThreshold(params.scoreThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setNegativeThreshold(params.unionNegativeThreshold)

      heBinary
        .setPositives(experiment.getPositives)
        .setNegatives(experiment.getNegatives)
        .igParallel(hypothesis.get).print()

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/imdb1.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testTrains1(): Array[String] = {
    val parameters = Params("trains1-toy").generateParams()
    val metaTransition0 = Parser.parseRule("r0(V0,V1) :- f(V1), g(V0,V1).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- f(V1), g(V0,V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- r0(V0, V1), r1(V0, V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- r1(V0, V2), r2(V0, V1).").get

    val results = parameters.map(params=>{
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

    val pw = new PrintWriter("resources/experiments/inventions/trains1.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testTrains2(): Array[String] = {
    val parameters = Params("trains2").generateParams()

    val metaTransition0 = Parser.parseRule("r0(V0,V1) :- g(V0,V1), f(V1), z(V1).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- g(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0, V1) :- r0(V0, V1), r1(V0, V1).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- r1(V0, V2), r2(V0, V1).").get
    val results = parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setScoreThreshold(params.scoreThreshold)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary))
    })

    val pw = new PrintWriter("resources/experiments/inventions/trains2.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def testTrains3(): Array[String] = {
    val parameters = Params("trains3-toy").generateParams()
    val metaTransition1 = Parser.parseRule("r1(V0,V1) :- l(V0,V1), f(V1).").get
    val metaTransition2 = Parser.parseRule("r2(V0) :- g(V0,V4), f(V4, V2).").get
    val metaTransition3 = Parser.parseRule("r3(V0) :- w(V0, V1), k(V0, V2).").get
    val metaTransition4 = Parser.parseRule("r4(V0) :- m(V0), n(V0).").get

    val results = parameters.map(params=>{
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
        .setScoreThreshold(params.scoreThreshold)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/trains3.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testIGGPAttritionNextScore(): Array[String] = {
    val parameters = Params("iggp-attrition-next-score").generateParams()

    val metaTransitionL0 = Parser.parseRule("p(G,Pl) :- s(Act), r(G,Opp,Act), u(Pl,Opp).").get
    val metaTransitionL1 = Parser.parseRule("p(G,Pl,Sc) :- k(G,Pl), q(G,Pl,Sc).").get
    val metaTransitionB0 = Parser.parseRule("p(G,Pl, Sc0) :- s(Act), r(G,Pl,Act), q(G,Pl,Sc0).").get
    val metaTransitionB1 = Parser.parseRule("p(G,Pl, Sc1) :- k(G,Pl, Sc0), t(Sc1,Sc0).").get
    val metaTransitionS0 = Parser.parseRule("p(G,Pl,Sc) :- q(G,Pl,Sc), r(G,Pl,Act), s(Act).").get

    val results =  parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      params.binaryPositiveThreshold = 0.0
      params.binaryNegativeThreshold = 1.0


      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransitionL0)
        .addMetaRule(metaTransitionL1)
        .addMetaRule(metaTransitionB0)
        .addMetaRule(metaTransitionB1)
        .addMetaRule(metaTransitionS0)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)

      //heBinary.setPositives(experiment.getPositives).setNegatives(experiment.getNegatives).igParallel(hypothesis).print()

      val heUnion = new UnionFast(engine)
        .setScoreThreshold(params.scoreThreshold)
        .setNegativeThreshold(0.1)
        .setPositiveThreshold(0.01)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/IGGPAttritionNextScore.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testIGGPCentipedeScore(): Array[String] = {
    val parameters = Params("iggp-gt_centipede-legal").generateParams()

    val metaTransitionL0 = Parser.parseRule("p(V0,V1,V2) :- pair(V0,V1), single(V2).").get
    val metaTransitionL1 = Parser.parseRule("p(V0,V1) :- pair(V0,V1), single(V1).").get
    //val metaTransitionC0 = Parser.parseRule("p(V0,V1,V2) :- pair(V0,V1, V2), tuple(V0,V1,V2).").get

    val metaTransitionC0 = Parser.parseRule("p(V0,V1,V2) :- found(V0,V3), single1(V1), single2(V2).").get

    val results =  parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      params.binaryPositiveThreshold = 0.0
      params.binaryNegativeThreshold = 1.0

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransitionL0)
        .addMetaRule(metaTransitionL1)
        .addMetaRule(metaTransitionC0)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)

      val heUnion = new UnionFast(engine)
        .setScoreThreshold(params.scoreThreshold)
        .setNegativeThreshold(0.000)
        .setPositiveThreshold(0.005)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/iggp-gt_centipede-legal.csv")
    pw.println(Params().toCSVHeaderLine())
    results.toArray.foreach(line => pw.println(line))
    pw.close()
    results.toArray
  }

  def testButtonsGoal(): Array[String] = {
    val parameters = Params("iggp-buttons-goal").generateParams()

    /*
    goal(V0,V1,V2):- int_0(V2),role(V1),not_my_true(V0,V3).
goal(V0,V1,V2):- int_100(V2),role(V1),prop_p(V4),my_true(V0,V4),prop_7(V3),my_true(V0,V3),prop_q(V5),my_true(V0,V5).
     */

    val metaTransitionL0 = Parser.parseRule("p(V0,V1,V2) :- pair(V0,V1), single(V2).").get
    val metaTransitionL1 = Parser.parseRule("p(V0,V1) :- pair(V0,V1), single(V1).").get
    val metaTransitionC0 = Parser.parseRule("p(V0,V1,V2) :- found(V0,V3), single1(V1), single2(V2).").get
    val metaTransitionC1 = Parser.parseRule("p(V0,V1,V2) :- pair1(V0,V1), single1(V2), pair2(V0,V2).").get

    val results =  parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      params.binaryPositiveThreshold = 0.0
      params.binaryNegativeThreshold = 1.0

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransitionL0)
        .addMetaRule(metaTransitionL1)
        .addMetaRule(metaTransitionC0)
        .addMetaRule(metaTransitionC1)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)

      val heUnion = new UnionFast(engine)
        .setScoreThreshold(params.scoreThreshold)
        .setNegativeThreshold(0.000)
        .setPositiveThreshold(0.005)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/iggp-buttons-goal.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testIGGPMinimalDecayScore(): Array[String] = {
    val parameters = Params("iggp-minimal-decay-next-value").generateParams()
    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- d(V0,V4,V2),l(V2).").get
    val metaTransition1 = Parser.parseRule("r1(V0, V1) :- m(V0, V4, V2), f(V1).").get
    val metaTransition2 = Parser.parseRule("r1(V0, V1, V2) :- w(V1, V2), k(V0, V2).").get
    val metaTransition3 = Parser.parseRule("r4(V0, V1) :- r(V0, V1, V2), n(V0, V4, V3).").get

    val results = parameters.map(params=>{
      val experiment = new Experiment(params).load()
      val engine = EngineParallel(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .setScoreThreshold(params.scoreThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(0.01)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/testIGGPMinimalDecayScore.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }


  def testIGGPChickenGoalScore(): Array[String] = {
    val parameters = Params("iggp-gt-chicken-goal-tiny").generateParams()
    val metaTransition0 = Parser.parseRule("r1(V0, V1, V2) :- m(V0,V2),l(V1).").get

    val results = parameters.map(params=>{
      //Condition
      //params.maxRules = 7
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

    val pw = new PrintWriter("resources/experiments/inventions/testIGGPChickenGoalScore.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testIGGPSokobanGoalScore(): Array[String] = {
    val parameters = Params("iggp-sokoban-goal").generateParams()

    val metaTransition0 = Parser.parseRule("r0(V0,V4,V5,V6) :- t(V0,V4,V6,V5),l(V5).").get
    val metaTransition1 = Parser.parseRule("r1(V0,V3,V4,V5,V6) :- f1(V0,V6,V4,V3),f2(V0,V6,V4,V5).").get
    val metaTransition2 = Parser.parseRule("r2(V0,V1,V2,V3) :- t1(V0,V1,V2), a1(V0,V1,V2,V3).").get
    val metaTransition3 = Parser.parseRule("r3(V0,V1,V2) :- r4(V0,V6,V4), r5(V0,V3,V4,V5,V6), rk(V1), rn(V2).").get

    val results = parameters.map(params=>{

      //params.iterationsSize = 20
      //params.filterSize = 1000
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


    val pw = new PrintWriter("resources/experiments/inventions/testIGGPSokobanGoalScore.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

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
    val metaTransition2 = Parser.parseRule("r5(V0, V2, V3, V4, V5) :- c(V3,V5,V2), a(V5,V0,V4).").get
    val metaTransition3 = Parser.parseRule("r2(V0, V1, V4) :- l(V0, V2, V3, V4, V5), r(V3,V0,V1).").get
    val metaTransition4 = Parser.parseRule("r3(V0) :- m(V0,V1,V4), c(V1), r(V4).").get

    val results = parameters.map(params=>{
      //params.filterSize = 10000
      params.maxRules = 50
      params.unionPositiveThreshold = 0.0005
      params.unionNegativeThreshold = 0.0
      val experiment = new Experiment(params).load()
        .pruneDatabase()
      val db = experiment.getDatabase
      val engine = EngineCache(db, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition0)
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

    val pw = new PrintWriter("resources/experiments/inventions/ptc.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results
  }

  def testPTE(): Array[String] = {
    val parameters = Params("pte").generateParams()
    val metaTransition0 = Parser.parseRule("r(V0) :- k(V1,V3), m(V1), x(V1,V3), a(V0,V3,V4,V2,V5).").get
    val metaTransition1 = Parser.parseRule("r(V0) :- o(V5,V1), k(V5,V1), a(V0,V1,V2,V4,V3).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- i(V5,V1), s(V5), a(V0,V1,V2,V4,V3).").get

    val results = parameters.map(params=>{

      params.binaryPositiveThreshold = 0.01
      params.binaryNegativeThreshold = 1.0
      params.unionPositiveThreshold = 0.01
      params.unionNegativeThreshold = 0.005
      params.scoreThreshold = 0.7
      //params.filterSize = 10000

      val experiment = new Experiment(params).load().pruneDatabase()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/pte.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

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

    val metaTransition1 = Parser.parseRule("rr(V0):- a(V0,V2), l(V0,V1).").get
    val metaTransition2 = Parser.parseRule("rr(V0):- e(V0,V1), r(V0,V1).").get
    val metaTransition3 = Parser.parseRule("rr(V0):- a(V2,V3), i(V3,V0, V1).").get
    val metaTransition4 = Parser.parseRule("rr(V0):- p(V0,V1), r(V0,V1).").get
    val metaTransition5 = Parser.parseRule("rr(V0):- p(V0,V4), i(V2,V0,V1), r(V3,V4).").get
    val metaTransition6 = Parser.parseRule("rr(V0):- p(V0,V3), r(V0,V2), r(V1,V3).").get
    val metaTransition7 = Parser.parseRule("rr(V0):- p(V0,V3), r(V2,V3), e(V2,V1).").get
    val metaTransition8 = Parser.parseRule("rr(V0):- p(V3,V0,V1), p(V3,V2), r(V3,V2).").get
    val metaTransition9 = Parser.parseRule("rr(V0):- p(V2,V1), i(V2,V0,V3), r(V0,V4).").get

    /*val metaTransition1 = Parser.parseRule("r(V0) :- p(V0, V2), k(V0,V1).").get
    val metaTransition2 = Parser.parseRule("r(V0) :- p(V0, V1), k(V0,V1).").get
    val metaTransition3 = Parser.parseRule("r(V0) :- p(V0, V3), k(V0,V2), e(V2,V1).").get
    val metaTransition4 = Parser.parseRule("r(V0) :- i(V3,V0,V1), p(V3,V2), rb(V3,V2).").get
    val metaTransition5 = Parser.parseRule("r(V0) :- p(V2,V1), i(V2,V0,V3), rb(V0,V4).").get*/

    val results  = parameters.map(params=>{
      //params.filterSize = 5000000
      params.binaryPositiveThreshold = 0.005
      params.binaryNegativeThreshold = 0.9
      params.unionPositiveThreshold = 0.001
      params.unionNegativeThreshold = 0.0

      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .addMetaRule(metaTransition5)
        .addMetaRule(metaTransition6)
        .addMetaRule(metaTransition7)
        .addMetaRule(metaTransition8)
        .addMetaRule(metaTransition9)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/yeast.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def testUWCS(): Array[String] = {
    val parameters = Params("uwcs").generateParams()

    val hypothesis = Parser.parseHypothesis("advisedBy(V0,V1):- ta(V3,V0,V4),taughtBy(V3,V1,V4),taughtBy(V5,V0,V2).\n"+
      "advisedBy(V0,V1):- student(V0),tempAdvisedBy(V3,V1),taughtBy(V4,V0,V5),taughtBy(V4,V1,V2).\n"+
      "advisedBy(V0,V1):- ta(V5,V0,V3),taughtBy(V5,V1,V3),tempAdvisedBy(V4,V1),publication(V2,V0),publication(V2,V1),publication(V2,V4).")

    val metaTransition1 = Parser.parseRule("r(V0, V1, V2, V4) :- a(V4, V0, V5), a(V4, V1, V2).").get
    val metaTransition2 = Parser.parseRule("r(V0, V1, V2, V3, V4) :- t(V0, V1, V2, V4), a(V3, V1).").get
    val metaTransition3 = Parser.parseRule("r(V0, V1) :- t(V0, V1, V2, V3, V4), s(V0).").get
    val metaTransition4 = Parser.parseRule("r(V0, V1, V2, V4) :- p(V2, V0), p(V2, V1), p(V2, V4).").get
    val metaTransition5 = Parser.parseRule("r(V0, V1, V2, V4) :- t(V4, V1), k(V0, V1, V2, V4).").get
    val metaTransition6 = Parser.parseRule("r(V0, V1) :- t(V5, V0, V3), a(V5, V1, V3), k(V0, V1, V2, V4).").get
    val results  =parameters.map(params => {
      params.unionNegativeThreshold = 0.0
      params.unionPositiveThreshold = 0.001

      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .addMetaRule(metaTransition5)
        .addMetaRule(metaTransition6)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)


      heBinary
        .setPositives(experiment.getPositives)
        .setNegatives(experiment.getNegatives)
        .igParallel(hypothesis.get).print()

      val heUnion = new UnionFast(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/uwcs.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def testWebkb(): Array[String] = {

    val parameters = Params("webkb").generateParams()
    val metaTransition0 = Parser.parseRule("r(C,P) :- co(C, P), pr(P).").get
    val metaTransition1 = Parser.parseRule("r(C1,C2,P) :- co(C1,P), pr(C2,P).").get
    val metaTransition2 = Parser.parseRule("r(C1,P1,P2) :- co(C1,P1), pr(C1,P2).").get
    val metaTransition3 = Parser.parseRule("r(C1,P1,P2) :- t(C1, P1, P2), a(C1,P1,P2).").get
    val metaTransition4 = Parser.parseRule("r(C1,C2, P1) :- t(C1, C2, P1), a(C1,P1,P2).").get
    val metaTransition5 = Parser.parseRule("r(C1,P1, P2) :- t(C1, P1, P2), a(C1,P1,P2).").get
    val metaTransition6 = Parser.parseRule("r(P1) :- t(C1, C2, P1), a(C1,P1,P2).").get
    val metaTransition7 = Parser.parseRule("r(P2) :- t(C1, C2, P1), a(C1,P1,P2).").get

    val results = parameters.map(params=>{

      val experiment = new Experiment(params).load().pruneDatabase()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new Binary(engine)
        .addMetaRule(metaTransition0)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .addMetaRule(metaTransition5)
        .addMetaRule(metaTransition6)
        .addMetaRule(metaTransition7)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      val heUnion = new UnionBinary(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/webkb.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def testZendo1(): Array[String] = {
    val parameters = Params("zendo1").generateParams()
    val metaTransition1 = Parser.parseRule("r(V0, V1, V2) :- co(V0, V2), pr(V2, V1).").get
    val metaTransition2 = Parser.parseRule("r(V2, V3) :- t(V2, V3), a(V3).").get
    val metaTransition3 = Parser.parseRule("r(V0, V1, V2) :- r1(V0, V1, V2), a(V1).").get
    val metaTransition4 = Parser.parseRule("r(V0) :- r1(V0, V1, V2), r2(V2, V3).").get

    val results = parameters.map(params=>{
      params.binaryPositiveThreshold = 0.1
      params.binaryNegativeThreshold = 0.9

      val experiment = new Experiment(params).load()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
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

    val pw = new PrintWriter("resources/experiments/inventions/zendo1.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def testZendo2(): Array[String] = {
    val parameters = Params("zendo2").generateParams()

    /*
      zendo(V0):- piece(V0,V1),green(V1),coord1(V1,V3),piece(V0,V2),lhs(V2),coord1(V2,V3).\n"+
      "zendo(V0):- piece(V0,V1),blue(V1),piece(V0,V3),green(V3),piece(V0,V2),red(V2).
     */

    val metaTransition1 = Parser.parseRule("re(V0, V1) :- pi(V0, V1), s(V1).").get
    val metaTransition2 = Parser.parseRule("re(V0, V2, V3) :- pi(V0, V2), c(V2,V3).").get
    val metaTransition3 = Parser.parseRule("re(V0, V1, V2, V3) :- co(V1, V3), ke(V0,V2,V3).").get
    val metaTransition4 = Parser.parseRule("re(V0) :- c1(V0, V1), ke(V0,V1,V2,V3).").get
    val metaTransition5 = Parser.parseRule("re(V0) :- c1(V0, V1), g1(V0,V3), r(V0,V2).").get

    /*
    val metaTransition1 = Parser.parseRule("re(V0, V1) :- pi(V0, V1), s(V1).").get
    val metaTransition4 = Parser.parseRule("re(V1, V2, V3) :- c(V1, V3), c(V2, V3).").get
    val metaTransition2 = Parser.parseRule("re(V0) :- r1(V0, V1), r2(V0, V2), r3(V0, V3).").get
    val metaTransition5 = Parser.parseRule("re(V0) :- r1(V0, V1), r2(V0, V2), r3(V1, V2, V3).").get
*/
    val results = parameters.map(params=>{
      //params.filterSize = 5000
      val experiment = new Experiment(params).load().pruneDatabase()
      val engine = EngineCache(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFast(engine)
        .addMetaRule(metaTransition1)
        .addMetaRule(metaTransition2)
        .addMetaRule(metaTransition3)
        .addMetaRule(metaTransition4)
        .addMetaRule(metaTransition5)
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

    val pw = new PrintWriter("resources/experiments/inventions/zendo2.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def testSynthesis(): Array[String] = {
    val parameters = Params("synthesis-next").generateParams()
    val metaRecursion = Parser.parseRule("re(V0, V1) :- pi(V0, V2), re(V2,V1).").get
      .buildRecursion()
    val metaRest1 = Parser.parseRule("re(V0, V1, V2) :- t(V0, V2), c(V1, V2).").get
    val metaRest2 = Parser.parseRule("re(V0, V1) :- rest(V0, V1, V2), r2(V3, V0), alpha(V3).").get

    val results = parameters.map(params=>{

      params.binaryPositiveThreshold = 0.0
      params.binaryNegativeThreshold = 0.7
      params.unionPositiveThreshold = 0.0
      params.unionNegativeThreshold = 0.0
      params.recursionSize = 15

      val experiment = new Experiment(params).load()
      val engine = EngineSerial(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFunctional(engine)
        .addMetaRule(metaRecursion)
        .addMetaRule(metaRest1)
        .addMetaRule(metaRest2)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      val heUnion = new UnionFunctional(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/synthesis.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }
  def testSynthesisContains(): Array[String] = {
    val parameters = Params("synthesis-contains").generateParams()
    val metaRecursion = Parser.parseRule("re(V0) :- pi(V0, V2), re(V2).").get
      .setRecursion(true)
      .buildRecursion()
    val metaRest1 = Parser.parseRule("re(V0) :- t(V0, V1), single(V1).").get

    val results = parameters.map(params=>{

      params.binaryPositiveThreshold = 0.0
      params.binaryNegativeThreshold = 0.7
      params.unionPositiveThreshold = 0.0
      params.unionNegativeThreshold = 0.0
      params.recursionSize = 15

      val experiment = new Experiment(params).load()
      val engine = EngineSerial(experiment.getDatabase, params.recursionSize)

      val heBinary = new BinaryFunctional(engine)
        .addMetaRule(metaRecursion)
        .addMetaRule(metaRest1)
        .setPositiveThreshold(params.binaryPositiveThreshold)
        .setNegativeThreshold(params.binaryNegativeThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      val heUnion = new UnionFunctional(engine)
        .setNegativeThreshold(params.unionNegativeThreshold)
        .setPositiveThreshold(params.unionPositiveThreshold)
        .setResembleThreshold(params.resembleThreshold)
        .setResembleWindow(params.resembleWindow)
        .setScoreThreshold(params.scoreThreshold)

      measureResult(experiment, Array(heBinary, heUnion))
    })

    val pw = new PrintWriter("resources/experiments/inventions/synthesis-contains.csv")
    pw.println(Params().toCSVHeaderLine())
    results.foreach(line => pw.println(line))
    pw.close()
    results

  }

  def main(args: Array[String]): Unit = {
    experiment()
  }