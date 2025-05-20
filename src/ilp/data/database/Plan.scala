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

  val bitsize = db.bitsize
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
    if statistics.contains(id) then statistics(id)
    else if maxMap.contains(id) then
      Statistics(predicate, Set(predicate)).init(maxMap(id))
    else
      Statistics(predicate, Set(predicate))
  }

  def getRowSizes(relations: Array[Predicate]): Map[Int, Int] =
    relations.zipWithIndex.map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val stats = getStatistics(predicate)
        if stats.isDefined then
          id -> stats.get.rowSize()
        else
          id -> 1
      }
      }
      .toMap

  def getMaxMinScore(attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map { case (statistic, predicate) => statistic.getActiveSize(predicate, next) }.max))
    val found = scores.map(pair => (pair._1, pair._2))
    found.sortBy(_._2).head

  def getMaxMinRelativeScore(current: Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map { case (statistic, predicate) => statistic.getRelativeRatio(predicate, current, next) }.max))
    val found = scores.map(pair => (pair._1, pair._2))
    found.sortBy(_._2).head

  def getMinMinRelativeScore(current: Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map { case (statistic, predicate) => statistic.getRelativeRatio(predicate, current, next) }.min))
    val found = scores.map(pair => (pair._1, pair._2))
    found.sortBy(_._2).head


  def optimizeByRecursive(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMaxMinScore(attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByRecursive(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByRelative(current: Variable, attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMinMinRelativeScore(current, attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => !variable.equalName(nextVariable))
      val newArray = nextHead +: optimizeByRelative(nextVariable, restVariables, body, tables)
      newArray

  def optimizeByRecursive(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => (current, 1.0) +: optimizeByRecursive(current, attributes.filter(variable => !variable.equals(current)), body, tables))
      .sortBy(array => array.foldRight(1.0) { case ((attribute, score), main) => {
        if attribute.isSymbol() then main
        else score * main
      }
      })
    array.head

  def optimizeByRelative(attributes: Array[Variable], body: Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => {
        (current, 1.0) +: optimizeByRelative(current, attributes.filter(variable => !variable.equals(current)), body, tables)
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
    Optimized(query, sorted, relations, bitsize).setData(dataMap)
      .initRows(rowMap)

  def optimizeRelative(query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.flatMap(predicate => getStatistics(predicate))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map { case (statistics, index) => statistics.predicate.identifier(index) -> statistics.data }
      .toMap
    val sorted = optimizeByRelative(attributes, relations, stats).map(_._1)
    Optimized(query, sorted, relations, bitsize).setData(dataMap)
      .initRows(rowMap)

  def optimizeRelative(maxMap: Map[Int, Map[Int, Double]], query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => getStatistics(maxMap, predicate))

    val dataMap = relations.zipWithIndex.flatMap{case(predicate, index) => {
      val statistics = getStatistics(predicate)
      if statistics.isDefined then Some(predicate.identifier(index) -> statistics.get.getData())
      else None
    }}.toMap

    val sorted = optimizeByRelative(attributes, relations, stats).map(_._1)
    val rowMap = getRowSizes(relations)
    Optimized(query, sorted, relations, bitsize).setData(dataMap)
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
  def optimizeRelative(query: Hypothesis): Array[Optimized] =
    val presorted = query.getSorted()
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
        }}.toMap

      val id = ruleHead.identifier()
      countMap = countMap.updated(id, map)
      })

    presorted.map(rule => optimizeRelative(countMap, rule))

  def optimizeNone(query: Query): Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.flatMap(predicate => getStatistics(predicate))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map { case (statistics, index) => statistics.predicate.identifier(index) -> statistics.data }
      .toMap
    val sorted = attributes
    Optimized(query, sorted, relations, bitsize)
      .setData(dataMap)
      .initRows(rowMap)

  def optimizeNone(query: Hypothesis): Array[Optimized] =
    val presorted = query.getSorted()
    presorted.map(rule=> optimizeNone(rule))

}
