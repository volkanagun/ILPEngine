package ilp.data.database

import ilp.data.Position
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet



class Index(val predicate: Predicate, var data:Array[Predicate], val bitsize:Int) {
  //Pairwise sorted trie
  var rowMap  = predicate.getPositions().map(position=>{
    position.getIndex() -> Map[Variable, Set[Int]]()
  }).toMap

  var rowBitmap  = predicate.getPositions().map(position=>{
    position.getIndex() -> Map[Variable, BitSet]()
  }).toMap

  var roaringBitmap  = predicate.getPositions().map(position=>{
    position.getIndex() -> Map[Variable, RoaringBitmap]()
  }).toMap

  var cudaBitmap  = predicate.getPositions().map(position=>{
    position.getIndex() -> Map[Variable, Array[Int]]()
  }).toMap


  def convertFromBitmapToRows(bitmap: Array[Int]): Array[Int] = {
    val indices = scala.collection.mutable.ArrayBuffer[Int]()
    for (i <- bitmap.indices) {
      val word = bitmap(i)
      for (b <- 0 until 32) {
        if (((word >>> b) & 1) != 0) {
          indices += (i * 32 + b) // little-endian: bit b of word i
        }
      }
    }
    indices.toArray
  }

  def convertToBitmapFromRows(indices: Array[Int]): Array[Int] = {
    val bitmap = Array.fill(bitsize)(0)
    val length = bitsize * 32
    for (idx <- indices if idx >= 0 && idx < length) {
      val wordIndex = idx / 32
      val bitIndex = idx % 32
      bitmap(wordIndex) |= (1 << bitIndex) // LSB first (little-endian)
    }
    bitmap
  }

  def buildCuda():this.type ={
    cudaBitmap = rowMap.map{case(id, map)=>{
      id -> map.map{case(variable, rows)=> (variable -> convertToBitmapFromRows(rows.toArray))}
    }}
    this
  }

  def addIndex(predicates: Set[Predicate]):this.type = {
    predicates.filter(predicate => !data.contains(predicate)).foreach(predicate => addData(predicate) addIndex(predicate))
    this
  }

  def addData(predicate: Predicate):this.type = {
    data = data :+ predicate
    this
  }

  def addIndex(predicate: Predicate, index:Int = data.size):this.type = {
    predicate.getPositions().foreach(position => {
      val i = position.getIndex()
      val trie = rowMap(i)
      val bitmap = rowBitmap(i)
      val roaringmap = roaringBitmap(i)
      val value = predicate.getVariable(position.index)
      val roadingBitmap = roaringmap.getOrElse(value, RoaringBitmap())

      roadingBitmap.add(index)

      rowMap = rowMap.updated(i, trie.updated(value, trie.getOrElse(value, Set()) + index))
      rowBitmap = rowBitmap.updated(i, bitmap.updated(value, bitmap.getOrElse(value, BitSet()).incl(index)))
      roaringBitmap = roaringBitmap.updated(i, roaringmap.updated(value, roadingBitmap))
    })
    this
  }

  def build(): this.type = {
    data.zipWithIndex.foreach{case(row, index)=>{
      addIndex(row, index)
    }}

    buildCuda()
  }

  def getValues(rows:Set[Int], position: Int):Set[Variable] =
    rows.map(index=> data(index)).map(predicate=> predicate.getVariable(position)).toSet


  def getValues(rows:BitSet, position: Int):Set[Variable] =
    rows.toSet.map(indice => data(indice))
      .map(predicate=> predicate.getVariable(position))

  def getValues(bitmap:Array[Int], position: Int):Set[Variable] = {
    val rows = convertFromBitmapToRows(bitmap)
    rows.map(index => data(index))
      .map(predicate=> predicate.getVariable(position)).toSet
  }

  def getValues(rows:RoaringBitmap, position: Int):Set[Variable] =
    rows.toArray.map(indice => data(indice))
      .map(predicate=> predicate.getVariable(position))
      .toSet


  def getRows(value:Variable, position: Int):Set[Int]=
    val trie = rowMap(position)
    trie.getOrElse(value, Set())

  def getCudaRows(value:Variable, position: Int):Array[Int]=
    val trie = cudaBitmap(position)
    trie(value)

  def getRows(rows:Set[Int], value:Variable, position: Int):Set[Int]=
    val trie = rowMap(position)
    val newRows = trie(value)
    newRows

  def getRows(rows:BitSet, value:Variable, position: Int):BitSet =
    val trie = rowBitmap(position)
    val newRows = trie(value)
    newRows.intersect(rows)

  def getRows(rows:RoaringBitmap, value:Variable, position: Int):RoaringBitmap =
    val trie = roaringBitmap(position)
    val existingRows = trie(value)
    RoaringBitmap.and(rows, existingRows)

}
