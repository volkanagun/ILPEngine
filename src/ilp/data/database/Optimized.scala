package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.data.{Query, Substitution}
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet

class Optimized(val query: Query, var variables: Array[Variable] = Array(), var predicates: Array[Predicate] = Array(), var bitSize: Int) {

  var rows: Map[Int, Set[Int]] = Map()
  var rowsBitmap: Map[Int, BitSet] = Map()
  var roaringBitmap: Map[Int, RoaringBitmap] = Map()
  var cudaBitmap: Map[Int, Array[Int]] = Map()
  var dataMap: Map[Int, Set[Predicate]] = Map()
  var isTarget:Boolean = false

  def identifier():Int =
    getHead().identifier()

  def newData(): Optimized = {
    Optimized(query, variables, predicates, bitSize)
  }

  def setTarget(isTarget:Boolean):this.type = {
    this.isTarget = isTarget
    this
  }

  def getTarget():Boolean = isTarget

  def newData(map: Map[Int, Set[Predicate]]): Optimized = {
    Optimized(query, variables, predicates, bitSize)
      .setData(map)
  }

  def hasHead(predicate: Predicate):Boolean =
    query.getHead().identifier() == predicate.identifier()

  def getQuery(): Query =
    query

  def getHead(): Predicate =
    query.getHead()

  def getVariables(): Array[Variable] =
    variables.filter(variable=> !variable.isSymbol())

  def getRelations(): Array[Predicate] =
    predicates

  def getDataMap(): Map[Int, Set[Predicate]] =
    dataMap

  def getBitmapMap(): Map[Int, BitSet] =
    rowsBitmap

  def getRoaringMap(): Map[Int, RoaringBitmap] =
    roaringBitmap

  def isRecursive(): Boolean =
    query.isRecursive()

  def queryId(): Int =
    query.hashCode()
/*

  def filter(ids: Array[(Predicate, Int)]): Optimized =
    val replaces = ids.zipWithIndex.map { case ((predicate, oldId), indice) => oldId -> predicate.identifier(indice) }
      .toMap
    val rels = ids.map(_._1)
    val newDataMap = replaces.map { case (oldId, newId) => newId -> dataMap(oldId) }
    val newRows = replaces.map { case (oldId, newId) => newId -> rows(oldId) }
    val newBitmap = replaces.map { case (oldId, newId) => newId -> rowsBitmap(oldId) }
    val newCudaBitmap = replaces.map { case (oldId, newId) => newId -> cudaBitmap(oldId) }
    val newRoaringBitmap = replaces.map { case (oldId, newId) => newId -> roaringBitmap(oldId) }
    val vars = rels.flatMap(_.getArray()).toSet
    val newVars = variables.filter(variable => vars.contains(variable))

    Optimized(query, newVars, rels, bitSize)
      .setData(newDataMap)
      .setRows(newRows)
      .setBitset(newBitmap)
      .setCudaBitmap(newCudaBitmap)
      .setRoaring(newRoaringBitmap)

*/

  /*
  def exclude(id: Int): Optimized =
    val ids = predicates.zipWithIndex.map { case (predicate, index) => {
      (predicate, predicate.identifier(index))
    }
    }.filter { case (predicate, _) => predicate.identifier() != id }
    filter(ids)*/

/*  def include(id: Int): Optimized =
    val ids = predicates.zipWithIndex.map { case (predicate, index) => {
      (predicate, predicate.identifier(index))
    }
    }.filter { case (predicate, _) => predicate.identifier() == id }
    filter(ids)*/

/*  def getRecursive(): Optimized = {
    val crrId = query.getHead().identifier()
    include(crrId)
  }*/

/*  def getNonRecursive(): Optimized =
    val crrId = query.getHead().identifier()
    exclude(crrId)*/

  def setRows(map: Map[Int, Set[Int]]): this.type =
    rows = map
    this

  def setBitset(map: Map[Int, BitSet]): this.type =
    rowsBitmap = map
    this

  def setRoaring(map: Map[Int, RoaringBitmap]): this.type =
    roaringBitmap = map
    this

  def setCudaBitmap(map: Map[Int, Array[Int]]): Optimized =
    cudaBitmap = map
    this

  def setData(map: Map[Int, Set[Predicate]]): Optimized = {
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

  def convert(indices: Array[Int]): Array[Int] = {
    val bitmap = Array.fill(bitSize)(0)
    val length = bitSize * 32
    for (idx <- indices if idx >= 0 && idx < length) {
      val wordIndex = idx / 32
      val bitIndex = idx % 32
      bitmap(wordIndex) |= (1 << bitIndex) // LSB first (little-endian)
    }
    bitmap
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
    rows = rows.updated(id, rows(id).filter(row => rowSet.contains(row)))
    rowsBitmap = rowsBitmap.updated(id, rowsBitmap(id).intersect(rowSet))
    cudaBitmap = cudaBitmap.updated(id, cudaBitmap(id).intersect(rowSet.toArray))

    val roaring = RoaringBitmap()
    roaring.add(rowSet.toArray: _*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }

  private def initRows(id: Int, max: Int): this.type = {
    rows = rows.updated(id, Range(0, max).toSet)
    rowsBitmap = rowsBitmap.updated(id, BitSet(Range(0, max): _*))
    cudaBitmap = cudaBitmap.updated(id, convert(Range(0, max).toArray))

    val roaring = RoaringBitmap()
    roaring.add(Range(0, max): _*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }


  override def toString = predicates.map(_.name).mkString(" & ") + "=>" + variables.mkString("[", ",", "]")
}
