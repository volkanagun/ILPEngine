package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.{Hypothesis, Query}

case class Key(predicate: Predicate, index: Int) {
  override def hashCode(): Int = predicate.identifier() * 7 + index

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Key]
    other.hashCode() == hashCode()
}

class Plan(val db: Database) {


  val statistics = db.getStatistics()

  def getStatistics(predicate: Predicate): Option[Statistics] = {
    val id = predicate.identifier()
    if statistics.contains(id) then
      Some(statistics(id)
        .setPredicate(predicate))
    else
      None
  }


  def getStatistics(maxMap: Map[Int, Map[Int, Double]], predicate: Predicate): Statistics = {
    val id = predicate.identifier()
    if statistics.contains(id) then
      statistics(id).setPredicate(predicate)
    else if maxMap.contains(id) then
      Statistics(predicate, Set(predicate)).init(maxMap(id))
    else
      Statistics(predicate, Set(predicate))
  }

  def getRowSizes(relations: Array[Predicate]): Map[Int, Int] =
    relations.zipWithIndex.flatMap { case (predicate, index) => {
        val id = predicate.identifier(index)
        val stats = getStatistics(predicate)
        if stats.isDefined then
          Some(id -> stats.get.rowSize())
        else
          None
      }
      }
      .toMap

  def getMaxMinScore(attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map { case (statistic, predicate) => statistic.getActiveSize(predicate, next) }.max))
    val found = scores.map(pair => (pair._1, pair._2))
    found.minBy(_._2)

  def getMaxMinRelativeScore(current: Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map { case (statistic, predicate) => statistic.getRelativeRatio(predicate, current, next) }.max))
    val found = scores.map(pair => (pair._1, pair._2))
    found.minBy(_._2)

  def getAvgMinRelativeScore(current: Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, {
      val scores = statistics.zip(body).map { case (statistic, predicate) => statistic.getRelativeRatio(predicate, current, next) }
      scores.sum / scores.length
    }))
    val found = scores.map(pair => (pair._1, pair._2))
    found.minBy(_._2)

  def getMinMinRelativeScore(current: Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map { case (statistic, predicate) => statistic.getRelativeRatio(predicate, current, next) }.min))
    val found = scores.map(pair => (pair._1, pair._2))
    found.minBy(_._2)

  def getMinMaxScore(current: Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, {
      val values = statistics.zip(body).map { case (statistic, predicate) => statistic.getRelativeRatio(predicate, current, next) }
      values.min
    }))
    val found = scores.map(pair => (pair._1, pair._2))
    found.maxBy(_._2)


  def optimizeByRecursive(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMaxMinScore(attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByRecursive(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByMinMin(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMinMinRelativeScore(current, attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByMinMin(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByExperimental(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMinMinRelativeScore(current, attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByExperimental(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByMaxMin(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMaxMinRelativeScore(current, attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByMaxMin(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByAvgMin(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getAvgMinRelativeScore(current, attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByAvgMin(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByRecursive(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => (current, 1.0) +: optimizeByRecursive(current, attributes.filter(variable => !variable.equals(current)), body, tables))
      .sortBy(array => array.foldRight(1.0) { case ((attribute, score), main) => {
        if attribute.isSymbol() then main
        else score * main
      }
      })
    array.head

  def optimizeMinMin(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => {
        (current, 1.0) +: optimizeByMinMin(current, attributes.filter(variable => !variable.equals(current)), body, tables)
      })
      .sortBy(array => array.foldRight(1.0) { case ((attribute, score), main) => {
        if attribute.isSymbol() then main
        else score * main
      }
      })
    array.head

  def optimizeExperimental(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val rowCounts = attributes.map(current=> tables.filter(stats => stats.hasVariable(current))
      .map(stats=> stats.getActiveSize(current)).maxOption.getOrElse(1.0))
    val zipAttributes = attributes.zip(rowCounts)
    val array = zipAttributes.map{case(current, rowCount) => {
      (current, rowCount) +: optimizeByExperimental(current, attributes.filter(variable => !variable.equals(current)), body, tables)
    }}.sortBy(array => array.foldRight(1.0) { case ((attribute, score), main) => {
      if attribute.isSymbol() then main
      else score * main
    }
    })
    array.head

  def optimizeMaxMin(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => {
        (current, 1.0) +: optimizeByMaxMin(current, attributes.filter(variable => !variable.equals(current)), body, tables)
      })
      .sortBy(array => array.foldRight(1.0) { case ((attribute, score), main) => {
        if attribute.isSymbol() then main
        else score * main
      }
      })
    array.head

  def optimizeAvgMin(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => {
        (current, 1.0) +: optimizeByAvgMin(current, attributes.filter(variable => !variable.equals(current)), body, tables)
      })
      .sortBy(array => array.foldRight(1.0) { case ((attribute, score), main) => {
        if attribute.isSymbol() then main
        else score * main
      }
      })
    array.head


  def optimize(query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.flatMap(predicate => getStatistics(predicate))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map { case (statistics, index) => statistics.predicate.identifier(index) -> statistics.data }
      .toMap
    val sorted = optimizeByRecursive(attributes, relations, stats).map(_._1)
    Optimized(query, sorted, relations).setData(dataMap)
      .initRows(rowMap)

  def optimizeRelative(query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.flatMap(predicate => getStatistics(predicate))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map { case (statistics, index) => statistics.predicate.identifier(index) -> statistics.data }
      .toMap
    val sorted = optimizeMinMin(attributes, relations, stats).map(_._1)
    Optimized(query, sorted, relations).setData(dataMap)
      .initRows(rowMap)

  def optimizeMinMin(maxMap: Map[Int, Map[Int, Double]], query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => getStatistics(maxMap, predicate))
    val sorted = optimizeMinMin(attributes, relations, stats).map(_._1)
    val rowMap = getRowSizes(relations)

    val dataMap = relations.zipWithIndex.flatMap { case (predicate, index) => {
      val statistics = getStatistics(predicate)
      if statistics.isDefined then Some(predicate.identifier(index) -> statistics.get.getData())
      else None
    }
    }.toMap


    Optimized(query, sorted, relations).setData(dataMap)
      .initRows(rowMap)

  def optimizeExperimental(maxMap: Map[Int, Map[Int, Double]], query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => getStatistics(maxMap, predicate))
    val statsMap = stats.map(stat => stat.identifier() -> stat).toMap
    val sorted = optimizeExperimental(attributes, relations, stats).map(_._1)
    val sortedRelations = relations.sortBy(predicate => statsMap(predicate.identifier()).getData().size)
    val dataMap = sortedRelations.zipWithIndex.flatMap { case (predicate, index) => {
      val statistics = getStatistics(predicate)
      if statistics.isDefined then Some(predicate.identifier(index) -> statistics.get.getData())
      else None
    }}.toMap

    val rowMap = getRowSizes(sortedRelations)
    Optimized(query, sorted, sortedRelations).setData(dataMap)
      .initRows(rowMap)

  def optimizeMaxMin(maxMap: Map[Int, Map[Int, Double]], query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => getStatistics(maxMap, predicate))

    val dataMap = relations.zipWithIndex.flatMap { case (predicate, index) => {
      val statistics = getStatistics(predicate)
      if statistics.isDefined then Some(predicate.identifier(index) -> statistics.get.getData())
      else None
    }
    }.toMap

    val sorted = optimizeMaxMin(attributes, relations, stats).map(_._1)
    val rowMap = getRowSizes(relations)
    Optimized(query, sorted, relations).setData(dataMap)
      .initRows(rowMap)

  def optimizeAvgMin(maxMap: Map[Int, Map[Int, Double]], query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => getStatistics(maxMap, predicate))

    val dataMap = relations.zipWithIndex.flatMap { case (predicate, index) => {
      val statistics = getStatistics(predicate)
      if statistics.isDefined then Some(predicate.identifier(index) -> statistics.get.getData())
      else None
    }
    }.toMap

    val sorted = optimizeAvgMin(attributes, relations, stats).map(_._1)
    val rowMap = getRowSizes(relations)
    Optimized(query, sorted, relations).setData(dataMap)
      .initRows(rowMap)


  /* //Fix here, a better code structure is needed...
   def optimizeRelative(query: Hypothesis): Array[Optimized] =
     val presorted = query.getSorted()
     val maxMap = presorted.map(rule => {
         val ruleHead = rule.getHead()
         val headVariables = ruleHead.getArray()
         val bodyStatistics = rule.getBody().filter(predicate => !predicate.equalByIdentifier(ruleHead)).flatMap(predicate => getStatistics(predicate))
         val map = headVariables.zipWithIndex.map { case (variable, position) => {
           val sizeCounts = bodyStatistics.filter(stats => stats.hasVariable(variable))
             .map(stats => stats.getActiveSize(variable))

           position -> sizeCounts.min
         }
         }.toMap
         ruleHead.identifier() -> map
       }).groupBy(pair => pair._1)
       .view.mapValues(array => {
         array.flatMap(_._2).groupBy(_._1).view
           .mapValues(item => item.map(_._2).max)
           .toMap
       }).toMap

     presorted.map(rule => optimizeRelative(maxMap, rule))*/

  //Fix here, a better code structure is needed...
  def optimizeMinMin(query: Hypothesis): Array[Optimized] =
    val presorted = query.getRanked()
    var countMap = Map[Int, Map[Int, Double]]()
    presorted.foreach(rule => {
      val ruleHead = rule.getHead()
      val headVariables = ruleHead.getArray()
      val bodyStatistics = rule.getBody().map(predicate => getStatistics(countMap, predicate))
      val map = headVariables.zipWithIndex.map { case (variable, position) => {
        val sizeCounts = bodyStatistics.filter(stats => stats.hasVariable(variable))
          .map(stats => stats.getActiveSize(variable))
        val minCount = if sizeCounts.nonEmpty then sizeCounts.min else 1
        position -> minCount
      }
      }.toMap

      val id = ruleHead.identifier()
      countMap = countMap.updated(id, map)
    })

    presorted.map(rule => optimizeMinMin(countMap, rule))

  def optimizeExperimental(query: Hypothesis): Array[Optimized] =
    val presorted = query.getRanked()
    var countMap = Map[Int, Map[Int, Double]]()
    presorted.foreach(rule => {
      val ruleHead = rule.getHead()
      val headVariables = ruleHead.getArray()
      val bodyStatistics = rule.getBody().map(predicate => getStatistics(countMap, predicate))
      val map = headVariables.zipWithIndex.map { case (variable, position) => {
        val sizeCounts = bodyStatistics.filter(stats => stats.hasVariable(variable))
          .map(stats => stats.getActiveSize(variable))
        val maxCount = if sizeCounts.nonEmpty then sizeCounts.max else 1
        position -> maxCount
      }}.toMap

      val id = ruleHead.identifier()
      countMap = countMap.updated(id, map)
    })

    presorted.map(rule => optimizeExperimental(countMap, rule)).map(optimized=>{
      val isTarget = query.getHead() == optimized.getHead()
      optimized.setTarget(isTarget)
    })

  def optimizeMaxMin(query: Hypothesis): Array[Optimized] =
    val presorted = query.getRanked()
    var countMap = Map[Int, Map[Int, Double]]()
    presorted.foreach(rule => {
      val ruleHead = rule.getHead()
      val headVariables = ruleHead.getArray()
      val bodyStatistics = rule.getBody().map(predicate => getStatistics(countMap, predicate))
      val map = headVariables.zipWithIndex.map { case (variable, position) => {
        val sizeCounts = bodyStatistics.filter(stats => stats.hasVariable(variable))
          .map(stats => stats.getActiveSize(variable))
        val minCount = if sizeCounts.nonEmpty then sizeCounts.min else 1
        position -> minCount
      }
      }.toMap

      val id = ruleHead.identifier()
      countMap = countMap.updated(id, map)
    })

    presorted.map(rule => optimizeMaxMin(countMap, rule))

  def optimizeAvgMin(query: Hypothesis): Array[Optimized] =
    val presorted = query.getRanked()
    var countMap = Map[Int, Map[Int, Double]]()
    presorted.foreach(rule => {
      val ruleHead = rule.getHead()
      val headVariables = ruleHead.getArray()
      val bodyStatistics = rule.getBody().map(predicate => getStatistics(countMap, predicate))
      val map = headVariables.zipWithIndex.map { case (variable, position) => {
        val sizeCounts = bodyStatistics.filter(stats => stats.hasVariable(variable))
          .map(stats => stats.getActiveSize(variable))
        val minCount = if sizeCounts.nonEmpty then sizeCounts.min else 1
        position -> minCount
      }
      }.toMap

      val id = ruleHead.identifier()
      countMap = countMap.updated(id, map)
    })

    presorted.map(rule => optimizeAvgMin(countMap, rule))

  def optimizeNone(query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributeArray()
    val stats = relations.flatMap(predicate => getStatistics(predicate))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map { case (statistics, index) => statistics.predicate.identifier(index) -> statistics.data }
      .toMap
    val sorted = attributes
    Optimized(query, sorted, relations)
      .setData(dataMap)
      .initRows(rowMap)

  def optimizeNone(query: Hypothesis): Array[Optimized] =
    val presorted = query.getRanked()
    presorted.map(rule => optimizeNone(rule)).map(optimized => {
      val isTarget = query.getHead() == optimized.getHead()
      optimized.setTarget(isTarget)
    })

}
