package ilp.cpu

import ilp.data.variables.Variable

import scala.util.Random

class CPUQueryPlan(var query:GPUQuery):

  def optimizeByNone():GPUQuery =
    println("Query optimization has started...")
    val newQuery = GPUQuery(query.id,query.maxValue, query.tables).setAttributes(query.attributes).init()
    println("Query optimization finished...")
    newQuery

  def optimizeByBranch():GPUQuery =
    println("Query optimization has started...")
    var tables = query.tables
    val maxValue = query.valueMax()
    val attributes = query.getAttributes().sortBy(variable=>{
      tables.filter(table=> table.contains(variable))
        .map(table => table.getActiveSize(variable)).max
    }).reverse

    tables = tables.sortBy(table=>{
      attributes.filter(variable=> table.contains(variable))
        .map(variable=> table.getActiveSize(variable)).max
    }).reverse


    val newQuery = GPUQuery(query.id,maxValue, tables).setAttributes(attributes).init()
    println("Query optimization finished...")
    newQuery

  def optimizeByNext():GPUQuery =
    println("Query optimization has started...")
    var tables = query.tables
    val maxValue = query.valueMax()
    tables = tables.sortBy(table=>{
      table.attributes.filter(variable=> table.contains(variable))
        .map(variable=> table.getRelationSize(variable)).max
    }).reverse

    val attributes = tables.flatMap(table=> table.attributes.sortBy(variable=>{
      table.getRelationSize(variable)
    }))

    val newQuery = GPUQuery(query.id,maxValue, tables).setAttributes(attributes).init()
    println("Query optimization finished...")
    newQuery

  def optimizeByAttr():GPUQuery =
    println("Query optimization has started...")
    var tables = query.tables
    val maxValue = query.valueMax()
    val attributes = query.getAttributes().map(attribute=> attribute -> tables.filter(table=> table.contains(attribute))
      .map(table=> table.getActiveSize(attribute)).max)
      .sortBy(_._2)
      .reverse

    tables = tables.sortBy(table=>{
      attributes.filter {pair=> table.contains(pair._1)}.map(pair=> pair._2).max
    })

    val variables = Random.shuffle(attributes.toSeq).toArray
    val newQuery = GPUQuery(query.id,maxValue, tables).setAttributes(variables.map(_._1)).init()
    println("Query optimization finished...")
    println("Attributes: " + variables.map(pair=> pair._1.getName() + "["+pair._2+"]").mkString(","))
    println("Table attributes: " + tables.map(t=> t.name+"("+t.attributes.mkString(",")+")").mkString(" "))
    newQuery

  def optimizeByDependency(current:Variable, attributes:Array[Variable], tables:Array[GPUTable]):(Variable,Double) =
    val scores = attributes.map(next=> (next, tables.map(table=> table.getDependencyScore(current, next)).max))
    val found = scores.map(pair => (pair._1 , pair._2))
    found.sortBy(_._2).head

  def optimizeByRecursive(current:Variable, attributes:Array[Variable], tables:Array[GPUTable]):Array[(Variable, Double)] =
    if attributes.isEmpty then Array()
    else
      val nextHead = optimizeByDependency(current, attributes, tables)
      val nextVariable = nextHead._1
      val restVariables = attributes.filter(variable=> variable!=nextVariable)
      val newArray = nextHead +: optimizeByRecursive(nextVariable, restVariables, tables)
      newArray

  def optimizeByRecursive(attributes:Array[Variable], tables:Array[GPUTable]):Array[(Variable, Double)] =
    val array = attributes.map(current => (current, 1.0) +: optimizeByRecursive(current, attributes.filter(variable=> variable!=current), tables))
      .sortBy(array=> array.foldRight(1.0){case(crr, main) => crr._2 * main})
    array.head


  def optimizeByDependency():GPUQuery =
    println("Query optimization has started...")
    val tables = query.tables
    val maxValue = query.valueMax()
    val attributes = query.getAttributes()
    val selected = optimizeByRecursive(attributes, tables)


    val newQuery = GPUQuery(query.id,maxValue, tables).setAttributes(selected.map(_._1)).init()
    println("Query optimization finished...")
    println("Selected scores: " + selected.map(pair=> pair._1.getName() + "[" + pair._2.toString + "]").mkString(","))

    println("Table attributes: " + tables.map(t=> t.name+"("+t.attributes.mkString(",")+")").mkString(" "))
    newQuery

  def optimizeByDepth():GPUQuery =
    println("Query optimization has started...")
    var tables = query.tables
    val maxValue = query.valueMax()
    val attributes = query.getAttributes().sortBy(variable=>{
      tables.filter(table=> table.contains(variable))
        .map(table => table.getActiveSet(variable).size).max
    })

    tables = tables.sortBy(table=>{
      attributes.filter(variable=> table.contains(variable))
        .map(variable=> table.getActiveSet(variable).size).max
    })


    val newQuery = GPUQuery(query.id,maxValue, tables).setAttributes(attributes).init()
    println("Query optimization finished...")
    newQuery
    //query

