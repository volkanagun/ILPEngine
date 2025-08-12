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


  private def addIndex(predicate: Predicate, index: Int): this.type = {

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

  def getHavingRows(rows: RoaringBitmap, valueHash: Int, position: Int): RoaringBitmap =
    val trie = roaringBitmap(position)

    if trie.containsKey(valueHash) then
      val existingRows = trie.get(valueHash)
      RoaringBitmap.and(rows, existingRows)
    else
      rows

}
