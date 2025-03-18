package ilp.gpu

import ilp.data.{Position, Query, Substitution}
import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.variables.Sym

class JoinEngine(database: Database):

  var tables = Map[Int, Array[Int]]()
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

  def addTable(predicate: Predicate): Unit = {
    val id = predicate.identifier()
    val values = addSymbolID(predicate.getSymbols())

    updateColCount(id, values.length)
    incRowCount(id)

    tables = tables.updated(id, tables.getOrElse(id, Array[Int]()) ++ values)
    predicateMap = predicateMap.updated(id, predicateMap.getOrElse(id, Array[Predicate]()) :+ predicate)
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

  def initialRowFilter(id: Int): Array[Int] =
    Array.fill[Int](rowCount(id))(1)

  def initialRowFilter(predicates:Array[Predicate]): Map[Int, Array[Int]] =
    predicates.zipWithIndex.map{case(p, pindex) => {
      val table_id = p.identifier()
      val p_id = p.identifier(pindex)
      p_id -> initialRowFilter(table_id)
    }}.toMap

  def join(positions: Array[Position], filterMap: Map[Int, Array[Int]]): Map[Int, Array[Int]] =
    if positions.size == 2 then
      val join = new JoinTwoWay()
      val t1 = positions(0).getIdentifier()
      val t2 = positions(1).getIdentifier()
      val tp1 = positions(0).getPositionIdentifier()
      val tp2 = positions(1).getPositionIdentifier()

      join.joinCols = positions.map(_.index)
      join.rowCount1 = rowCount(t1)
      join.rowCount2 = rowCount(t2)
      join.colLength1 = colCount(t1)
      join.colLength2 = colCount(t2)
      join.table1 = tables(t1)
      join.table2 = tables(t2)
      join.rowFilter1 = filterMap(tp1)
      join.rowFilter2 = filterMap(tp2)
      join.init()
      //JoinManager.run(join.rowCount1, join.rowCount2, join)
      join.runFlat()
      getRowFilter(join.result,Array(join.rowCount1, join.rowCount2), Array(tp1, tp2))
    else if (positions.size == 3) then
      val join = new JoinThreeWay()
      val t1 = positions(0).getIdentifier()
      val t2 = positions(1).getIdentifier()
      val t3 = positions(2).getIdentifier()
      val tp1 = positions(0).getPositionIdentifier()
      val tp2 = positions(1).getPositionIdentifier()
      val tp3 = positions(2).getPositionIdentifier()

      join.joinCols = positions.map(_.index)
      join.rowCount1 = rowCount(t1)
      join.rowCount2 = rowCount(t2)
      join.rowCount3 = rowCount(t3)
      join.colLength1 = colCount(t1)
      join.colLength2 = colCount(t2)
      join.colLength3 = colCount(t3)
      join.table1 = tables(t1)
      join.table2 = tables(t2)
      join.table3 = tables(t3)
      join.rowFilter1 = filterMap(t1)
      join.rowFilter2 = filterMap(t2)
      join.rowFilter3 = filterMap(t3)
      join.init()
      JoinManager.run(join.rowCount1, join.rowCount2, join.rowCount3, join)
      getRowFilter(join.result,Array(join.rowCount1, join.rowCount2, join.rowCount3), Array(tp1, tp2, tp3))
    else
      filterMap

  //Take more rows then the table
  def getRowFilter(result: Array[Int], rowCounts:Array[Int], pids: Array[Int]): Map[Int, Array[Int]] =
    var newMap = pids.zip(rowCounts).map{case(pid, rcount)=> pid -> Array.fill[Int](rcount)(-1)}.toMap
    val size = result.length-pids.length

    for (i <- 0 until size by pids.length)
      for (t <- 0 until pids.length) {
        val indice = i + t
        val r = result(indice)
        if r>=0.0 then
          newMap(pids(t))(r) = 1
      }
    newMap

  def get(pindex:Int, predicate: Predicate, crrRowIndices:Array[Int]):Array[Int]=
    val valids = crrRowIndices.zipWithIndex.filter(_._1>=0).map(_._2)
    if predicate.isNegative() && valids.isEmpty then
      initialRowFilter(predicate.identifier(pindex))
    else
      valids


  def join(query: Query): Set[Substitution] =
    val predicates = query.getBody()
    val positions = query.getBodyPosition().filter(_._2.length > 1)
    var filterMap = initialRowFilter(predicates)
    positions.foreach { case (p, set) => {
      val crrFilter = join(set, filterMap)
      filterMap ++= crrFilter
    }}

    var substitutions = Set[Substitution](Substitution())
    predicates.zipWithIndex.foreach { case (predicate, pindex) => {
      val id = predicate.identifier()
      val position_id = predicate.identifier(pindex)
      val positions = predicate.getPositions(-1)
      val variables = positions.map(_.getVariable())
      val crrRowIndices = get(pindex, predicate, filterMap(position_id))
      val d = 0;
      substitutions = crrRowIndices.map(rowIndex => predicateMap(id)(rowIndex)).flatMap(predicate => {
        val values = positions.map(position => predicate.getVariable(position.getIndex()))
        val newSubstitution = Substitution(variables, values)
        substitutions.map(subs => subs.appendNew(newSubstitution))
      }).toSet
    }}

    substitutions


