package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet


class Index(val predicate: Predicate, var data: Array[Predicate], val bitsize: Int = 128) {

  var rowMap = predicate.getPositions().map(position => {
    position.getIndex() -> Map[Variable, Set[Int]]()
  }).toMap

  var roaringBitmap = predicate.getPositions().map(position => {
    position.getIndex() -> Map[Variable, RoaringBitmap]()
  }).toMap

  def addIndex(predicates: Set[Predicate]): this.type = {
    predicates.toArray.zipWithIndex.foreach{case(predicate, index) => {
      addData(predicate)
      addIndex (predicate, index)
    }}
    this
  }

  def addData(predicate: Predicate): this.type = {
    data = data :+ predicate
    this
  }

  def addIndex(predicate: Predicate, index: Int): this.type = {

    predicate.getPositions().foreach(position => {
      val i = position.getIndex()
      val trie = rowMap(i)
      val roaringmap = roaringBitmap(i)
      val value = predicate.getVariable(position.index)
      val roadingBitmap = roaringmap.getOrElse(value, RoaringBitmap())

      roadingBitmap.add(index)

      rowMap = rowMap.updated(i, trie.updated(value, trie.getOrElse(value, Set()) + index))
      roaringBitmap = roaringBitmap.updated(i, roaringmap.updated(value, roadingBitmap))

    })
    this
  }

  def build(): this.type = {
    data.zipWithIndex.foreach { case (row, index) => {
      addIndex(row, index)
    }}
    this
  }

  def getValues(rows: Set[Int], position: Int): Set[Variable] =
    rows.map(index => data(index)).map(predicate => predicate.getVariable(position))


/*
  def getValues(rows: BitSet, position: Int): Set[Variable] =
    rows.toSet.map(indice => data(indice))
      .map(predicate => predicate.getVariable(position))
*/


  def getValues(rows: RoaringBitmap, position: Int): Set[Variable] =
    rows.toArray.map(indice => data(indice))
      .map(predicate => predicate.getVariable(position))
      .toSet


  def getRows(value: Variable, position: Int): Set[Int] =
    val trie = rowMap(position)
    trie.getOrElse(value, Set())
/*

  def getRows(rows: Set[Int], value: Variable, position: Int): Set[Int] =
    val trie = rowMap(position)
    val newRows = trie(value)
    newRows
*/

  def getRows(rows: RoaringBitmap, value: Variable, position: Int): RoaringBitmap =
    val trie = roaringBitmap(position)
    val existingRows = trie(value)
    RoaringBitmap.and(rows, existingRows)

  def getHavingRows(rows: RoaringBitmap, value: Variable, position: Int): RoaringBitmap =
    val trie = roaringBitmap(position)
    if trie.contains(value) then
      val existingRows = trie(value)
      RoaringBitmap.and(rows, existingRows)
    else
      rows

}
