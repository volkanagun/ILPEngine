package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.{Query, Substitution}
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet

class Optimized(val query: Query, var variables: Array[Variable] = Array(), var predicates: Array[Predicate] = Array()) extends Serializable{

  var rows: Map[Int, Set[Int]] = Map()
  var roaringBitmap: Map[Int, RoaringBitmap] = Map()
  var dataMap: Map[Int, Array[Predicate]] = Map()
  val queryId = query.hashCode()
  val headId = query.getHead().identifier()
  var isTarget:Boolean = false

  def identifier():Int =
    headId

  def newData(): Optimized = {
    Optimized(query, variables, predicates)
  }

  def setTarget(isTarget:Boolean):this.type = {
    this.isTarget = isTarget
    this
  }

  def getTarget():Boolean = isTarget

  def newData(map: Map[Int, Array[Predicate]]): Optimized = {
    Optimized(query, variables, predicates)
      .setData(map)
  }

  def hasRecursion(predicate: Predicate):Boolean =
    query.getHead().identifier() == predicate.identifier()

  def getQuery(): Query =
    query

  def getHead(): Predicate =
    query.getHead()

  def getHeadCopy(): Predicate =
    query.getHead().copy().asPredicate()
/*

  def getVariables(): Array[Variable] =
    variables.filter(variable=> !variable.isSymbol())
*/

  def getVariables(): Array[Variable] =
    variables //.filter(variable=> variable.isVariable())

  def getRelations(): Array[Predicate] =
    predicates

  def getDataMap(): Map[Int, Array[Predicate]] =
    dataMap

  def getDataArrayMap(): Map[Int, Array[Predicate]] =
    dataMap

  def getRoaringMap(): Map[Int, RoaringBitmap] =
    roaringBitmap

  def isRecursive(): Boolean =
    query.isRecursive()

  def getQueryId(): Int =
    queryId

  def setRows(map: Map[Int, Set[Int]]): this.type =
    rows = map
    this

  def setRoaring(map: Map[Int, RoaringBitmap]): this.type =
    roaringBitmap = map
    this

  def setData(map: Map[Int, Array[Predicate]]): Optimized = {
    this.dataMap = map
    this
  }

  def substitution(predicate: Predicate): Substitution =
    val head = getHead()
    val replaces = head.getVariables()
      .zip(predicate.getVariables())
      .map { case (variable, sym) => (variable, sym.setName(variable.getName())) }

    Substitution(replaces)

  def substitution(substitution: Substitution): Optimized = {
    variables = variables.map(variable => {
      if substitution.hasVariable(variable) then {
        val newvariable = substitution.valueByVariable(variable).get
        newvariable.setName(variable.getName())
      }
      else variable
    })
    this
  }

  def setVariables(variables: Array[Variable]): this.type = {
    this.variables = variables
    this
  }

  def setRelations(tables: Array[Predicate]): this.type = {
    this.predicates = tables
    this
  }

  def initRows(map: Map[Int, Int]): this.type = {
    map.foreach { case (id, row) => initRows(id, row) }
    this
  }

  def filterRows(map: Map[Int, Set[Int]]): this.type = {
    map.foreach { case (id, rows) => filterRows(id, rows) }
    this
  }

  private def filterRows(id: Int, rowSet: Set[Int]): this.type = {
    rows = rows.updated(id, rows(id).intersect(rowSet))
    val roaring = RoaringBitmap()
    roaring.add(rowSet.toArray: _*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }

  private def initRows(id: Int, max: Int): this.type = {
    rows = rows.updated(id, Range(0, max).toSet)
    val roaring = RoaringBitmap()
    roaring.add(Range(0, max): _*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }


  override def toString = predicates.map(_.name).mkString(" & ") + "=>" + variables.mkString("[", ",", "]")
}
