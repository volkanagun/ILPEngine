package ilp.data.optimization

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.program.{Query, Substitution}
import org.roaringbitmap.RoaringBitmap

final class Optimized(val query: Query, var variables: Array[Variable] = Array(), var predicates: Array[Predicate] = Array()) extends Serializable{

  var rows: Map[Int, Set[Int]] = Map()
  var roaringBitmap: Map[Int, RoaringBitmap] = Map()
  var dataMap: Map[Int, Array[Predicate]] = Map()
  val queryId: Int = query.hashCode()
  val headId: Int = query.getHeadIdentifier
  var isTarget:Boolean = false

  inline def identifier():Int =
    headId

  inline def newData(): Optimized = {
    Optimized(query, variables, predicates)
  }

  inline def setTarget(isTarget:Boolean):this.type = {
    this.isTarget = isTarget
    this
  }

  inline def getTarget:Boolean = isTarget
  inline def getQuery: Query = query
  inline def getHead: Predicate = query.getHead
  inline def getVariables: Array[Variable] = variables
  inline def getRelations: Array[Predicate] = predicates
  inline def getDataMap: Map[Int, Array[Predicate]] =  dataMap
  inline def getRoaringMap: Map[Int, RoaringBitmap] = roaringBitmap
  inline def isRecursive: Boolean = query.isRecursive
  inline def isFunctional:Boolean = query.isFunctional
  inline def getQueryId: Int = queryId

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

  private def initRows(id: Int, max: Int): this.type = {
    rows = rows.updated(id, Range(0, max).toSet)
    val roaring = RoaringBitmap()
    roaring.add(Range(0, max): _*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }


  override def toString: String = predicates.map(_.name).mkString(" & ") + "=>" + variables.mkString("[", ",", "]")
}
