package ilp.cpu

import ilp.data.Parser
import ilp.data.database.{Engine, CudaManager, Optimized, Plan}
import ilp.experiments.{Experiment, Params}


object Performance:

  def measureTime[T](block: => T, name: String = "GPU"): T = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    val elapsedTime = (end - start) / 1e6
    println(s"${name} time in milliseconds: ${elapsedTime}")
    result
  }

  def measureMultipleTime[T](block: => T, name: String = "GPU", count: Int = 5): T = {
    val time = Range(0, count).map(i => {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      val elapsedTime = (end - start) / 1e6
      elapsedTime

    }).min
    println(s"${name} time in milliseconds: ${time}")
    block
  }



  def testFactTime(): Unit = {
    val params = Params("imdb3-toy")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("relevant(X,Y):-movie(X,M1) & movie(Y,M2) & genre(M1,G) & genre(M2,G).").get.compile()
    val result = measureTime(db.facts(q))
    result.foreach(subs => println(subs))
    println("Size: " + result.size)

  }


  def testLeapfrogBatchGPU(): Unit = {
    val params = Params("iggp-hextforthree-next-control")
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("path(X, Y, X1, Y1, Y2) :- adjacent(X, Y, X1, Y1) & adjacent(X1, Y1, X2, Y2).").get.compile()

    val engine = CPUEngine(db).compile()
    val optimized = engine.optimizeByDependency(q)
    val result = measureTime(engine.joinBatchParallel(optimized))
    println("Size: " + result.size)
  }

  def compareSerialWithParallel(name: String, batchSize: Int = 1000, localSize: Int = 4): Unit = {

    println("Performance comparison for " + name)
    println("Batch size " + batchSize)
    println("-----------------------------------")

    val params = Params(name)
    val exp = Experiment(params).load()
    val db = exp.database

    val q = Parser.parseRule("product(B1, P1, MA) :- predicate0(A, P1, S1, C1) & predicate1(P1, S2, M2, C1) " +
      "& predicate2(P1, M1, D1, D2)  " +
      "& predicate3(P1, M1, D1, D2) " +
      "& predicate4(A, B, D1, D2) " +
      "& predicate5(D1, B, A, D2).").get.compile()

    val engine = CPUEngine(db, batchSize, localSize).compile(q)
    val optimizedDepth = engine.optimizeByDepth(q)
    val optimizedDependency = engine.optimizeByDependency(q)
    val optimizedBranch = engine.optimizeByBranch(q)
    val optimizedNone = engine.optimizeNone(q)

    println("Current query: " + q)
    println("Flat query: " + optimizedNone)
    println("Depth query: " + optimizedDepth)
    println("Branch query: " + optimizedBranch)
    println("Dependency query: " + optimizedDependency)

    val resultCPUSerial = measureTime(engine.join(optimizedDependency), "Serial CPU")
    //val resultCPUParallel = measureMultipleTime(engine.joinParallel(optimizedDependency), "Parallel CPU", 2)

    CudaManager.setGPU()
    //engine.registerQuery(optimizedDependency)
    engine.registerQuery(optimizedDependency)

    val resultGPUParallel = measureTime(engine.joinBatchParallel(optimizedDependency), "Batch GPU")
    //val resultHETParallel = measureMultipleTime(engine.joinHeterogeneous(optimizedDependency), "Heterogeneous GPU", 1)

    println("Size: " + resultCPUSerial.size)
    //println("Test parallel: " + (resultCPUParallel.size == resultCPUSerial.size))
    //println("Test heterogeneous: " + (resultHETParallel.size == resultCPUSerial.size))
    println("Test gpu: " + (resultGPUParallel.size == resultCPUSerial.size))
    println("-----------------------------------")
    //engine.disposeQuery(optimizedDependency)
    engine.disposeQuery(optimizedDependency)
    System.gc()
  }


  def compareIndexing(name: String): Unit = {

    println("Performance comparison for " + name)
    println("-----------------------------------")

    val params = Params(name)
    val exp = Experiment(params).load()
    val db = exp.database

    val q = {
      if name.startsWith("dunn") then
        Parser.parseRule("product(B1, P1, MA) :- transaction(B1, P1, S1) & product(P1, M1, D1) & causal(P1, S2, M2).").get.compile()
      else
        Parser.parseRule("product(B1, P1, MA) :- predicate0(A, P1, S1, C1) & predicate1(P1, S2, M2, C1) " +
          "& predicate2(P1, M1, D1, D2)  " +
          "& predicate3(P1, M1, D1, D2) " +
          "& predicate4(A, B, D1, D2) " +
          "& predicate5(D1, B, A, D2).").get.compile()
    }

    val indexEngine = Engine(db)
    val noIndexEngine = CPUEngine(db).compile(q)

    val plan = Plan(db)
    //val optimizedNoIndex = noIndexEngine.optimizeByDependency(q)
    val bestDependency = plan.optimize(q)//.setVariables(optimizedNoIndex.getAttributes())
    val relativeDependency = plan.optimizeRelative(q)//.setVariables(optimizedNoIndex.getAttributes())

    //optimizedNoIndex.setAttributes(optimizedDependency.variables)

    println("Current query: " + q)
    println("Best query: " + bestDependency)
    println("Relative query: " + relativeDependency)

    println("Best query scores")
    println("==================")

    val resultCPUNoIndex = measureMultipleTime(indexEngine.joinData(bestDependency), "No index CPU", 2)
    System.gc()
    val resultCPUIndex = measureMultipleTime(indexEngine.joinBitmap(bestDependency), "Bitmap CPU", 2)
    System.gc()
    val resultRoaringIndex = measureMultipleTime(indexEngine.joinRoaring(bestDependency), "Roaring Bitmap CPU", 2)
    System.gc()
    //val resultCudaIndex = measureMultipleTime(indexEngine.joinCuda(bestDependency), "Cuda Bitmap CPU", 2)

    println("Relative query scores")
    println("==================")

    val relativeCPUNoIndex = measureMultipleTime(indexEngine.joinData(relativeDependency), "No index CPU", 2)
    System.gc()
    val relativeCPUIndex = measureMultipleTime(indexEngine.joinBitmap(relativeDependency), "Bitmap CPU", 2)
    System.gc()
    val relativeRoaringIndex = measureMultipleTime(indexEngine.joinRoaring(relativeDependency), "Roaring Bitmap CPU", 2)
    System.gc()
    //val relativeCudaIndex = measureMultipleTime(indexEngine.joinCuda(relativeDependency), "Cuda Bitmap CPU", 2)


    println("Size: " + resultCPUIndex.size)
    println("Test roaring: " + (resultCPUNoIndex.size == relativeRoaringIndex.size))
    //println("Test cuda: " + (resultCPUNoIndex.size == relativeCudaIndex.size))
    System.gc()
  }

  def testMultiParallelQuery(name: String, batchSize: Int = 32, localSize: Int = 1): Unit = {

    println("Performance comparison for " + name)
    println("Batch size " + batchSize)
    println("-----------------------------------")

    val params = Params(name)
    val exp = Experiment(params).load()
    val db = exp.database

    val q1 = Parser.parseRule("product(B1, P1, MA) :- transaction(B1, P1, S1) & product(P1, M1, D1) & causal(P1, S2, M2).").get.compile()
    val q2 = Parser.parseRule("product(B1, P1, M2) :- product(P1, M1, D1) & causal(P1, S2, D2).").get.compile()
    val q3 = Parser.parseRule("product(B1, P1, M2) :- transaction(B1, P1, S1) & causal(P1, S1, M2).").get.compile()
    val q4 = Parser.parseRule("product(B1, P1, M2) :- product(P1, M1, D1) & transaction(B1, P1, M2).").get.compile()
    val q5 = Parser.parseRule("product(B1, P1, MA) :- product(P1, M1, D1) & transaction(B1, P1, S1) & causal(P1, S2, M2).").get.compile()
    val q6 = Parser.parseRule("product(B1, P1, MA) :- product(P1, M1, D1) & causal(P1, S2, M2) & transaction(B1, P1, S1).").get.compile()
    val q7 = Parser.parseRule("product(B1, P1, MA) :- causal(P1, S2, M2) & transaction(B1, P1, S1) & product(P1, M1, D1).").get.compile()
    val engine = CPUEngine(db, batchSize, localSize)
    val array = Array(q1, q2, q3, q4, q5, q6, q7)
    val queries = array.map(r => engine.compile(r).optimizeByDependency(r))

    lazy val blockSerial = {
      engine.registerQuery(queries(0)).joinBatchParallel(queries(0))
      engine.registerQuery(queries(1)).joinBatchParallel(queries(1))
      engine.registerQuery(queries(2)).joinBatchParallel(queries(2))
      engine.registerQuery(queries(3)).joinBatchParallel(queries(3))
      engine.registerQuery(queries(4)).joinBatchParallel(queries(4))
      engine.registerQuery(queries(5)).joinBatchParallel(queries(5))
      engine.registerQuery(queries(6)).joinBatchParallel(queries(6))
    }

    CudaManager.setCPU()
    val resultCPUParallel = measureTime(blockSerial, "Parallel GPU in serial")
    CudaManager.setGPU()
    val resultGPUParallel = measureTime(engine.joinBatchParallel(queries), "Parallel GPU in parallel")

    println("-----------------------------------")
  }

  def compare(): Unit = {
    Range(0, 1).foreach(_ =>
      Range (0, 5).reverse.foreach {i=>
        val name = "random" + i
        //compareSerialWithParallel(name, 100)
        compareIndexing(name)
      })

  }

  def main(args: Array[String]): Unit = {
    compare()
  }