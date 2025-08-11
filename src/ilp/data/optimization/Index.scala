package ilp.data.optimization

import ilp.data.predicates.Predicate
import ilp.data.variables.{Num, Sym, Variable}
import org.roaringbitmap.RoaringBitmap

import java.util


final class Index(val predicate: Predicate, var data: Array[Predicate], val bitsize: Int = 128) extends Serializable{

  var rowMap: Map[Int, util.HashMap[Variable, Set[Int]]] = predicate.getPositions.map(position => {
    position.getIndex -> new util.HashMap[Variable, Set[Int]]()
  }).toMap

  var roaringBitmap: Map[Int, util.HashMap[Int, RoaringBitmap]] = predicate.getPositions.map(position => {
    position.getIndex -> new util.HashMap[Int, RoaringBitmap]()
  }).toMap


/*  def addIndex(predicates: Array[Predicate]): this.type = {
    predicates.zipWithIndex.foreach{case(predicate, index) => {
      addData(predicate)
      addIndex (predicate, index)
    }}
    this
  }*/
/*
  def addData(predicate: Predicate): this.type = {
    data = data :+ predicate
    this
  }*/

  def addIndex(predicate: Predicate, index: Int): this.type = {

    predicate.getPositions.foreach(position => {
      val i = position.getIndex
      val trie = rowMap(i)
      val value = predicate.getVariable(position.index)

      val set = if trie.containsKey(value) then trie.get(value)+index else Set(index)
      trie.put(value, set)

      val valueHash = value.hashCode()
      val roaringmap = roaringBitmap(i)
      val roadingBitmap = if roaringmap.containsKey(valueHash) then roaringmap.get(valueHash)
      else RoaringBitmap()
      roadingBitmap.add(index)
      roaringmap.put(valueHash, roadingBitmap)

      rowMap = rowMap.updated(i, trie)
      roaringBitmap = roaringBitmap.updated(i, roaringmap)

    })
    this
  }

  def build(): this.type = {
    data.zipWithIndex.foreach { case (row, index) => {
      addIndex(row, index)
    }}
    this
  }

/*  def getValues(rows: Set[Int], position: Int): Set[Variable] =
    rows.map(index => data(index)).map(predicate => predicate.getVariable(position))*/


/*  def getValues(rows: RoaringBitmap, position: Int): Set[Variable] =
    rows.toArray.map(indice => data(indice))
      .map(predicate => predicate.getVariable(position))
      .toSet*/
/*  def getRows(value: Variable, position: Int): Set[Int] =
    val trie = rowMap(position)
    if trie.containsKey(value) then trie.get(value)
    else Set()*/
/*

  def getRows(rows: Set[Int], value: Variable, position: Int): Set[Int] =
    val trie = rowMap(position)
    val newRows = trie(value)
    newRows
*/

/*  def getRows(rows: RoaringBitmap, value: Variable, position: Int): RoaringBitmap =
    val trie = roaringBitmap(position)
    val existingRows = trie.get(value)
    RoaringBitmap.and(rows, existingRows)*/

  def getHavingRows(rows: RoaringBitmap, valueHash: Int, position: Int): RoaringBitmap =
    val trie = roaringBitmap(position)

    if trie.containsKey(valueHash) then
      val existingRows = trie.get(valueHash)
      RoaringBitmap.and(rows, existingRows)
    else
      rows

}
