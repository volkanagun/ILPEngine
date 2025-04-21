package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet

class Optimized(var variables : Array[Variable] = Array(), var predicates:Array[Predicate] = Array()) {

  var rows:Map[Int, Set[Int]] = Map()
  var rowsBitmap:Map[Int, BitSet] = Map()
  var roaringBitmap:Map[Int, RoaringBitmap] = Map()
  var dataMap:Map[Int, Set[Predicate]] = Map()

  def setData(map:Map[Int, Set[Predicate]]):this.type = {
    this.dataMap = map
    this
  }

  def setVariables(variables:Array[Variable]):this.type = {
    this.variables = variables
    this
  }

  def setRelations(tables:Array[Predicate]):this.type = {
    this.predicates = tables
    this
  }


  def initRows(map:Map[Int, Int]):this.type = {
    map.foreach{case(id, row)=> initRows(id, row)}
    this
  }

  private def initRows(id:Int, max:Int):this.type = {
    rows = rows.updated(id, Range(0, max).toSet)
    rowsBitmap = rowsBitmap.updated(id, BitSet(Range(0, max):_*))

    val roaring = RoaringBitmap()
    roaring.add(Range(0, max):_*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }


  override def toString = predicates.map(_.name).mkString(" & ") + "=>" + variables.mkString("[",",","]")
}
