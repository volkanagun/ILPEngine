package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet

class Optimized(var variables : Array[Variable] = Array(), var predicates:Array[Predicate] = Array(), var bitSize:Int) {

  var rows:Map[Int, Set[Int]] = Map()
  var rowsBitmap:Map[Int, BitSet] = Map()
  var roaringBitmap:Map[Int, RoaringBitmap] = Map()
  var cudaBitmap:Map[Int, Array[Int]] = Map()

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


  def initRows(map:Map[Int, Int]):this.type = {
    map.foreach{case(id, row)=> initRows(id, row)}
    this
  }

  private def initRows(id:Int, max:Int):this.type = {
    rows = rows.updated(id, Range(0, max).toSet)
    rowsBitmap = rowsBitmap.updated(id, BitSet(Range(0, max):_*))
    cudaBitmap = cudaBitmap.updated(id, convert(Range(0, max).toArray))

    val roaring = RoaringBitmap()
    roaring.add(Range(0, max):_*)
    roaringBitmap = roaringBitmap.updated(id, roaring)
    this
  }


  override def toString = predicates.map(_.name).mkString(" & ") + "=>" + variables.mkString("[",",","]")
}
