package ilp.gpu


import ilp.data.{Position, Query, Substitution}
import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.variables.{Sym, Variable}

class JoinEngine(database: Database):

  var tables = Map[Int, Array[Int]]()
  var values = Map[Int, Array[Int]]()
  var predicateMap = Map[Int, Array[Predicate]]()

  var colCount = Map[Int, Int]()
  var rowCount = Map[Int, Int]()

  var string2id = Map[String, Int]()
  var id2string = Map[Int, String]()

  def compile(): this.type =
    database.sets.foreach(predicate => {
      addTable(predicate)
    })
    this

  def addSymbolID(value: String): Int =
    if !string2id.contains(value) then
      val id = string2id.size
      string2id = string2id.updated(value, id)
      id2string = id2string.updated(id, value)
      id
    else
      string2id(value)

  def addSymbolID(value: Array[Sym]): Array[Int] =
    value.map(sym => addSymbolID(sym.value))

  def addValues(positions: Array[Position], symbols: Array[Int]): Unit = {
    val crrMap = positions.zip(symbols).foreach { case (position, value) => {
      val id = position.getValueIdentifier()
      val crrArr = values.getOrElse(id, Array[Int]()) :+ value
      values = values.updated(id, crrArr)
    }}
  }

  def addTable(predicate: Predicate): Unit = {
    val id = predicate.identifier()
    val positions = predicate.getPositions()
    val values = addSymbolID(predicate.getSymbols())


    addValues(positions, values)
    updateColCount(id, values.length)
    incRowCount(id)


    tables = tables.updated(id, tables.getOrElse(id, Array[Int]()) ++ values)
    predicateMap = predicateMap.updated(id, predicateMap.getOrElse(id, Array[Predicate]()) :+ predicate)
  }

  def product(arrays: Array[Array[Int]]): Array[Array[Int]] = {
    arrays.foldLeft(Array(Array.empty[Int])) { (acc, array) =>
      for {
        list <- acc
        element <- array
      } yield list :+ element
    }
  }
  def product(arrays: Array[Array[Variable]]): Array[Array[Variable]] = {
    arrays.foldLeft(Array(Array.empty[Variable])) { (acc, array) =>
      for {
        list <- acc
        element <- array
      } yield list :+ element
    }
  }

  def updateColCount(id: Int, size: Int): Unit = {
    if !colCount.contains(id) then
      colCount = colCount.updated(id, size)
  }

  def incRowCount(id: Int): Unit = {
    if !rowCount.contains(id) then
      rowCount = rowCount.updated(id, 1)
    else
      rowCount = rowCount.updated(id, rowCount(id) + 1)
  }

  def join(variable:String, positions: Array[Position], context: Map[Int, Array[Int]]): JoinResult =
    if positions.size == 2 then
      val join = new JoinTwoWay()
      val position1 = positions(0)
      val position2 = positions(1)
      val p1 = position1.getPredicate()
      val p2 = position2.getPredicate()
      val t1 = position1.getIdentifier()
      val t2 = position2.getIdentifier()
      val index1 = Range(0, p1.getArity()).toArray
      val index2 = Range(0, p2.getArity()).toArray

      val tv1 = p1.getVariables().map(p => context(p.hashCode()))
      val tv2 = p2.getVariables().map(p => context(p.hashCode()))

      join.constraintColSize = Array[Int](p1.getArity(), p2.getArity())
      join.constraintCols = Array[Array[Int]](index1, index2)
      join.joinCols = positions.map(_.index)
      join.rowCount1 = rowCount(t1)
      join.rowCount2 = rowCount(t2)
      join.colLength1 = colCount(t1)
      join.colLength2 = colCount(t2)
      join.table1 = tables(t1)
      join.table2 = tables(t2)
      join.values1 = tv1
      join.values2 = tv2
      join.init()
      //JoinManager.run(join.rowCount1, join.rowCount2, join)
      join.runFlat()
      val valueMap = getValueFilter(context, Array(join.result1,join.result2), Array(p1, p2))
      val rowMap = getRows(variable, join.rows, Array(position1, position2))
      JoinResult(valueMap, rowMap)
    else if (positions.size == 3) then
      val join = new JoinThreeWay()
      val pos1 = positions(0)
      val pos2 = positions(1)
      val pos3 = positions(2)

      val p1 = pos1.getPredicate()
      val p2 = pos2.getPredicate()
      val p3 = pos3.getPredicate()

      val index1 = Range(0, p1.getArity()).toArray
      val index2 = Range(0, p2.getArity()).toArray
      val index3 = Range(0, p3.getArity()).toArray

      val t1 = pos1.getIdentifier()
      val t2 = pos2.getIdentifier()
      val t3 = pos3.getIdentifier()

      val tv1 = p1.getVariables().map(p => context(p.hashCode()))
      val tv2 = p2.getVariables().map(p => context(p.hashCode()))
      val tv3 = p3.getVariables().map(p => context(p.hashCode()))

      join.joinCols = positions.map(_.index)
      join.constraintColSize = Array[Int](p1.getArity(), p2.getArity(), p3.getArity())
      join.constraintCols = Array[Array[Int]](index1, index2, index3)
      join.rowCount1 = rowCount(t1)
      join.rowCount2 = rowCount(t2)
      join.rowCount3 = rowCount(t3)
      join.colLength1 = colCount(t1)
      join.colLength2 = colCount(t2)
      join.colLength3 = colCount(t3)
      join.table1 = tables(t1)
      join.table2 = tables(t2)
      join.table3 = tables(t3)
      join.values1 = tv1
      join.values2 = tv2
      join.values3 = tv3
      join.init()
      JoinManager.run(join.rowCount1, join.rowCount2, join.rowCount3, join)
      val valueMap = getValueFilter(context, Array(join.result), Array(p1, p2, p3))
      val rowMap = getRows(variable, Array(join.rows1, join.rows2, join.rows3), Array(pos1, pos2, pos3))
      JoinResult(valueMap, rowMap)
    else
      JoinResult()

  def getNegativeInit(context: Map[Int, Array[Int]], predicates: Array[Array[Position]]): Map[Int, Array[Int]] = {
    var map = context
    predicates.foreach(p=>{
      p.foreach(position=>{
        val id = position.getVariableIdentifier()
        val size = map(id).size
        map = map.updated(id, Array.fill[Int](size)(-1))
      })
    })
    map
  }

  def fullRows(identifier: Int): Array[Int] =
    val size = rowCount(identifier)
    Range(0, size).toArray

  def initialRows(items: Array[(String, Array[Position])]): Map[String, JoinRow] =
    items.map { case (variableName, positions) => {

      val pos_ids = positions.map(_.getPredicateIdentifier()).distinct
      val rws = positions.map(position => fullRows(position.getIdentifier()))
      val aff = positions.flatMap(position=> position.getPositions())
      val cartesian = product(rws)
      variableName -> JoinRow(variableName, pos_ids, aff, cartesian)
    }}.toMap


  def initialValues(positions: Array[Position]): Map[Int, Array[Int]] =
    positions.groupBy(_.getVariableIdentifier())
      .view
      .mapValues(positions => positions.map(position => values(position.getValueIdentifier()).max + 1)
        .max).mapValues(size => Array.fill[Int](size)(1)).toMap

  def getRows(variable:String, rows: Array[Array[Int]], positions:Array[Position]):Map[String, JoinRow] =
    val predicates = positions.map(p=> p.getPredicateIdentifier())
    val cartesian = rows.zipWithIndex.flatMap {case(source, sourceIndex) => source.zipWithIndex.filter(pair => pair._1 == 1)
      .map(_._2).map(destinationIndex=> Array(sourceIndex, destinationIndex))}
    Map(variable -> JoinRow(variable,predicates, positions, cartesian))

  def getValueFilter(context: Map[Int, Array[Int]], results: Array[Array[Int]], predicates: Array[Predicate]): Map[Int, Array[Int]] =
    val indices = Range(0, predicates.length).toArray
    val positions = predicates.map(_.getPositions())
    val map = getNegativeInit(context, positions)
    results.zip(predicates).zip(positions).foreach{case((result, predicate), position)=>{
      result.sliding(predicate.getArity(), predicate.getArity()).foreach(array=>{
        array.zip(position).foreach{case(value, p)=>{
          if value >= 0 then map(p.getVariableIdentifier())(value) = 1
        }}
      })
    }}
    map


  def join(query: Query): Set[Substitution] =

    val positions = query.getJoinPositions()
    val all = positions.flatMap(_._2)
    val valueMap =  initialValues(all)
    val rowMap = initialRows(positions)
    val mainResult = JoinResult(valueMap, rowMap)

    positions.foreach { case (p, set) => {
      val crrResult = join(p, set, valueMap)
      mainResult.add(crrResult)
    }}


    val substitutions = Set(Substitution())



    substitutions


