package ilp.gpu

import ilp.cpu.CPUEngine
import ilp.data.Parser
import ilp.experiments.{Experiment, Params}

object JoinTest:

  def measureTime[T](block: => T): T = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    val elapsedTime = (end - start) / 1e6
    println(s"Time in milliseconds: ${elapsedTime}")
    result
  }

  def testKinship(): Unit = {
    val params = Params("kinship-pi")
    val exp = Experiment(params).load()
    val db = exp.database
    val jb = JoinEngine(db).compile()
    val q = Parser.parseRule("anchestor(X,Y):-father(X,Z) & mother(Z,Y).").get.compile()
    jb.join(q).foreach(subs=> println(subs))
  }

  def testJoinTime(): Unit = {
    val params = Params("imdb3-toy")
    val exp = Experiment(params).load()
    val db = exp.database
    val jb = JoinEngine(db).compile()
    val q = Parser.parseRule("relevant(M1,M2):-genre(M1,G) & genre(M2,G).").get.compile()
    val result = measureTime(jb.join(q))
    result.foreach(subs=> println(subs))
    println("Size: "+result.size)
  }

  def testFactTime(): Unit = {
    val params = Params("imdb3-toy")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("relevant(X,Y):-movie(X,M1) & movie(Y,M2) & genre(M1,G) & genre(M2,G).").get.compile()
    val result = measureTime(db.facts(q))
    result.foreach(subs=> println(subs))
    println("Size: " + result.size)

  }

  def testLeapfrog(): Unit = {
    val params = Params("imdb3-toy")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("relevant(X,Y):-movie(X,M1) & movie(Y,M2) & genre(M1,G) & genre(M2,G).").get.compile()
    val engine = CPUEngine(db).compile()
    val result = measureTime(engine.join(q))
    println("Size: " + result.size)

  }

  def compareLeapfrogNoisy(): Unit = {

    System.setProperty("aparapi.device", "cpu")

    val params = Params("noisy-drugdrug")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("cyclic_query(D1, D2, E, T) :- enzyme(E, D1) & enzymeinhibitor(D1, E) & target(T, D1) & target(T, D2).").get.compile()
    val engine = CPUEngine(db).compile()
    val result1 = measureTime(engine.join(q))
    val result2 = measureTime(engine.joinBaseGPU(q))
  }

  def testLeapfrogGPU(): Unit = {
    val params = Params("iggp-hextforthree-next-control")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("path(X, Y, X1, Y1, Y2) :- adjacent(X, Y, X1, Y1) & adjacent(X1, Y1, X2, Y2).").get.compile()
    val engine = CPUEngine(db).compile()
    val result = measureTime(engine.joinBaseStackGPU(q))
    println("Size: " + result.size)

  }

  def testLeapfrogBatchGPU(): Unit = {
    val params = Params("iggp-hextforthree-next-control")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("path(X, Y, X1, Y1, Y2) :- adjacent(X, Y, X1, Y1) & adjacent(X1, Y1, X2, Y2).").get.compile()
    val engine = CPUEngine(db).compile()
    val result = measureTime(engine.joinBatchGPU(q))
    println("Size: " + result.size)

  }

  def main(args: Array[String]): Unit = {
    ///testLeapfrogGPU()
    testLeapfrogBatchGPU()

  }