package ilp.data.optimization

case class Edge(from: Int, to: Int, weight: Double)

object BellmanFordCycle extends Serializable {

  def apply(array: Array[Array[Double]]): (Array[Double], Array[Int]) =
    val n = array.length
    val edges = for {
      i <- 0 until n
      j <- 0 until n if i != j && array(i)(j) != Double.PositiveInfinity
    } yield Edge(i, j, -math.log(array(i)(j)))


    val edgeSize = edges.groupBy(_.from).view.mapValues(_.size).toMap

    val variables = Range(0, n).toSet
    var map = Map[Int, Double]()
    for (i <- 0 until n) {
      val sorted = bellmanFord(variables, edges.toList, i).toArray
        .filter(_._2 != Double.PositiveInfinity).sortBy(_._2)
      val avg = sorted.map(_._2).min
      //val avg = sorted.map(_._2).sum / sorted.size
      map = map.updated(i, avg)
    }

    val results = map.toArray.sortBy(_._2)
    (results.map(_._2),results.map(_._1))

  def applyDirect(array: Array[Array[Double]]): (Array[Double], Array[Int]) =
    val n = array.length
    val edges = for {
      i <- 0 until n
      j <- 0 until n if i != j && array(i)(j) != Double.PositiveInfinity
    } yield Edge(i, j, array(i)(j))


    val edgeSize = edges.groupBy(_.from).view.mapValues(_.size).toMap

    val variables = Range(0, n).toSet
    var map = Map[Int, Double]()
    for (i <- 0 until n) {
      val sorted = bellmanFord(variables, edges.toList, i).toArray
        .filter(_._2 != Double.PositiveInfinity).sortBy(_._2)
      val avg = sorted.map(_._2).sum / sorted.length
      map = map.updated(i, avg)
    }

    val results = map.toArray.sortBy(_._2)
    (results.map(_._2),results.map(_._1))


  def applySorted(array: Array[Array[Double]]): (Array[Double], Array[Int]) =
    val n = array.length
    val edges = for {
      i <- 0 until n
      j <- 0 until n if i != j && array(i)(j) != Double.PositiveInfinity
    } yield Edge(i, j, -math.log(array(i)(j)))


    val edgeSize = edges.groupBy(_.from).view.mapValues(_.size).toMap

    val variables = Range(0, n).toSet

    var finalset = Array[Array[(Int, Double)]]()
    var map = Map[Int, Double]()
    var start = (0,0d)
    for (i<-0 until n) {
      start = (i,0d)
      var considered = Range(0, n).toSet
      var taken = Array[(Int, Double)]()
      var takenSet = Set[Int]()
      taken = taken :+ start
      takenSet = takenSet + start._1
      considered = considered.filter(item => item != start._1)

      while considered.nonEmpty do {
        val sorted = bellmanFord(variables, edges.toList, start._1).toArray
          .filter(item => item._2 != Double.PositiveInfinity && !takenSet.contains(item._1)).sortBy(_._2)
        if sorted.nonEmpty then
          start = sorted.head
        else
          start = (considered.head, 0d)

        taken = taken :+ start
        takenSet = takenSet + start._1
        considered = considered.filter(item => item != start._1)

      }

      finalset :+= taken
    }

    val results = finalset.maxBy(items => items.map(_._2).sum)
    (results.map(_._2),results.map(_._1))


  private def bellmanFord(variables: Set[Int], edges: List[Edge], source: Int): Map[Int, Double] = {
    val dist = collection.mutable.Map[Int, Double](variables.toSeq.map(_ -> Double.PositiveInfinity): _*)
    dist(source) = 0.0

    for (_ <- 1 to variables.size) {
      for (Edge(u, v, w) <- edges) {
        if (dist(u) + w < dist(v)) {
          dist(v) = dist(u) + w
        }
      }
    }

    dist.toMap
  }

  def bellmanFord(n: Int, edges: List[Edge], source: Int): (Array[Double], Option[List[Int]]) = {
    val dist = Array.fill(n)(Double.PositiveInfinity)
    val pred = Array.fill(n)(-1)
    dist(source) = 0.0

    var lastUpdated = -1

    // Relax edges up to n times (n-th for cycle detection)
    for (_ <- 1 to n) {
      lastUpdated = -1
      for (Edge(u, v, w) <- edges) {
        if (dist(u) + w < dist(v)) {
          dist(v) = dist(u) + w
          pred(v) = u
          lastUpdated = v
        }
      }
    }

    // If lastUpdated is not -1, a negative cycle was detected
    if (lastUpdated != -1) {
      // Step 1: Move back n times to enter the cycle
      var cycleNode = lastUpdated
      for (_ <- 0 until n) {
        cycleNode = pred(cycleNode)
      }

      // Step 2: Reconstruct the cycle
      val cycle = scala.collection.mutable.ListBuffer[Int]()
      val visited = scala.collection.mutable.Set[Int]()
      var current = cycleNode

      // walk until we revisit a node
      while (!visited.contains(current)) {
        visited += current
        cycle.prepend(current)
        current = pred(current)
      }
      cycle.prepend(current) // close the cycle

      (dist, Some(cycle.toList))
    } else {
      (dist, None)
    }
  }
}

object Example extends App {
  val rates = Array(
    Array(1.0, 0.9, 0.8),
    Array(1.1, 1.0, 0.75),
    Array(1.25, 1.35, 1.0)
  )

  val n = rates.length
  val edges = for {
    i <- 0 until n
    j <- 0 until n if i != j
  } yield Edge(i, j, -math.log(rates(i)(j)))

  val (_, cycleOpt) = BellmanFordCycle.bellmanFord(n, edges.toList, 0)

  cycleOpt match {
    case Some(cycle) =>
      println("Negative-weight cycle (arbitrage opportunity) found:")
      println("Cycle path: " + cycle.mkString(" -> "))
    case None =>
      println("No arbitrage opportunity found.")
  }
}
