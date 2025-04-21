package ilp.data.database

import ilp.data.Query
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Plan(val db:Database) {

  val statistics = db.getStatistics()

  def getRowSizes(relations:Array[Predicate]):Map[Int, Int]=
    relations.zipWithIndex.map { case (predicate, index) => predicate.identifier(index) -> statistics(predicate.identifier()).rowSize() }
      .toMap

  def getMaxMinScore(attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map{case(statistic, predicate) => statistic.getActiveSize(predicate, next)}.max))
    val found = scores.map(pair => (pair._1, pair._2))
    found.sortBy(_._2).head

  def getMaxMinRelativeScore(current:Variable, attributes: Array[Variable], body: Array[Predicate], statistics: Array[Statistics]): (Variable, Double) =
    val scores = attributes.map(next => (next, statistics.zip(body).map{case(statistic, predicate) => statistic.getRelativeRatio(predicate, current, next)}.max))
    val found = scores.map(pair => (pair._1, pair._2))
    found.sortBy(_._2).head

  def optimizeByRecursive(current: Variable, attributes: Array[Variable],body:Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMaxMinScore(attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => variable != nextVariable)
      val newArray = nextHead +: optimizeByRecursive(nextVariable, restVariables,body, tables)
      newArray

  def optimizeByRelative(current: Variable, attributes: Array[Variable],body:Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = getMaxMinRelativeScore(current, attributes, body, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable => variable != nextVariable)
      val newArray = nextHead +: optimizeByRelative(nextVariable, restVariables,body, tables)
      newArray

  def optimizeByRecursive(attributes: Array[Variable], body:Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => (current, 1.0) +: optimizeByRecursive(current, attributes.filter(variable => !variable.equals(current)),body, tables))
      .sortBy(array => array.foldRight(1.0) { case (crr, main) => crr._2 * main })
    array.head

  def optimizeByRelative(attributes: Array[Variable], body:Array[Predicate], tables: Array[Statistics]): Array[(Variable, Double)] =
    val array = attributes.map(current => (current, 1.0) +: optimizeByRelative(current, attributes.filter(variable => !variable.equals(current)),body, tables))
      .sortBy(array => array.foldRight(1.0) { case (crr, main) => crr._2 * main })
    array.head


  def optimize(query: Query):Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => statistics(predicate.identifier()))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map{case(statistics, index) => statistics.predicate.identifier(index) -> statistics.data}
      .toMap
    val sorted = optimizeByRecursive(attributes,relations, stats).map(_._1)
    Optimized(sorted, relations).initRows(rowMap)
      .setData(dataMap)

  def optimizeRelative(query: Query):Optimized =
    val relations = query.getBody()
    val attributes = query.getAttributes().toArray
    val stats = relations.map(predicate => statistics(predicate.identifier()))
    val rowMap = getRowSizes(relations)
    val dataMap = stats.zipWithIndex.map{case(statistics, index) => statistics.predicate.identifier(index) -> statistics.data}
      .toMap
    val sorted = optimizeByRelative(attributes,relations, stats).map(_._1)
    Optimized(sorted, relations).initRows(rowMap)
      .setData(dataMap)

}
