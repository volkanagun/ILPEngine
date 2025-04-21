package ilp.data.database

import ilp.data.Position
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet



class Index(val predicate: Predicate, val data:Array[Predicate]) {
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


  def build(): this.type = {
    data.zipWithIndex.foreach{case(row, index)=>{
      row.getPositions().foreach(position=> {
        val i = position.getIndex()
        val trie = rowMap(i)
        val bitmap = rowBitmap(i)
        val roaringmap = roaringBitmap(i)
        val value = row.getVariable(position.index)
        val roadingBitmap = roaringmap.getOrElse(value, RoaringBitmap())

        roadingBitmap.add(index)

        rowMap = rowMap.updated(i, trie.updated (value, trie.getOrElse(value, Set()) + index))
        rowBitmap = rowBitmap.updated(i, bitmap.updated(value, bitmap.getOrElse(value, BitSet()).incl(index)))
        roaringBitmap = roaringBitmap.updated(i, roaringmap.updated(value, roadingBitmap))
      })
    }}
    this
  }

  def getValues(rows:Set[Int], position: Int):Set[Variable] =
    rows.map(index=> data(index)).map(predicate=> predicate.getVariable(position)).toSet


  def getValues(rows:BitSet, position: Int):Set[Variable] =
    rows.toSet.map(indice => data(indice))
      .map(predicate=> predicate.getVariable(position))

  def getValues(rows:RoaringBitmap, position: Int):Set[Variable] =
    rows.toArray.map(indice => data(indice))
      .map(predicate=> predicate.getVariable(position))
      .toSet


  def getRows(value:Variable, position: Int):Set[Int]=
    val trie = rowMap(position)
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
