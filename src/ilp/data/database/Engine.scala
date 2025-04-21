package ilp.data.database

import ilp.data.{Query, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

class Engine(val database: Database) {

  val dataIndex = database.getIndex()

  def filter(rowMap: Map[Int, Set[Int]], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, Set[Int]] =
    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val crrIndex = dataIndex(predicate.identifier())
        val crrRows = rowMap(id)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          val newRows = crrIndex.getRows(crrRows, value, indice)
          id -> newRows
        else
          id -> crrRows
      }}.toMap

    newMap

  def filterData(dataMap: Map[Int, Set[Predicate]], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, Set[Predicate]] =
    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val crrData = dataMap(id)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          val newRows = crrData.filter(predicate=> predicate.getVariable(indice) == value)
          id -> newRows
        else
          id -> crrData
      }}.toMap

    newMap

  def filterBitmap(rowMap: Map[Int, BitSet], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, BitSet] =
    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val crrIndex = dataIndex(predicate.identifier())
        val crrRows = rowMap(id)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          val newRows = crrIndex.getRows(crrRows, value, indice)
          id -> newRows
        else
          id -> crrRows
      }}.toMap

    newMap

  def filterRoaring(rowMap: Map[Int, RoaringBitmap], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, RoaringBitmap] =
    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val crrIndex = dataIndex(predicate.identifier())
        val crrRows = rowMap(id)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          val newRows = crrIndex.getRows(crrRows, value, indice)
          id -> newRows
        else
          id -> crrRows
      }}.toMap

    newMap

  def active(rowMap: Map[Int, Set[Int]], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val id = predicate.identifier(index)
        val rows = rowMap(id)
        dataIndex(predicate.identifier()).getValues(rows, predicate.getIndex(attribute))
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def activeData(dataMap: Map[Int, Set[Predicate]], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val id = predicate.identifier(index)
        val indice = predicate.getIndex(attribute)
        dataMap(id).map(predicate => predicate.getVariable(indice))
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def activeBitmap(rowMap: Map[Int, BitSet], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val id = predicate.identifier(index)
        val bitset = rowMap(id)
        dataIndex(predicate.identifier()).getValues(bitset, predicate.getIndex(attribute))
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }
  def activeRoaring(rowMap: Map[Int, RoaringBitmap], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val id = predicate.identifier(index)
        val bitset = rowMap(id)
        dataIndex(predicate.identifier()).getValues(bitset, predicate.getIndex(attribute))
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def join(map: Map[Int, Set[Int]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = active(map, relations, nextAttribute)

      activeDomain.flatMap(value => {
        val filteredMap = filter(map, relations, nextAttribute, value)
        val partialResults = join(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), nextAttribute)
        })
        results
      })

  def joinData(map: Map[Int, Set[Predicate]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = activeData(map, relations, nextAttribute)

      activeDomain.par.flatMap(value => {
        val filteredMap = filterData(map, relations, nextAttribute, value)
        val partialResults = joinData(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), nextAttribute)
        })
        results
      }).toArray.toSet

  def joinBitmap(map: Map[Int, BitSet], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = activeBitmap(map, relations, nextAttribute)

      activeDomain.par.flatMap(value => {
        val filteredMap = filterBitmap(map, relations, nextAttribute, value)
        val partialResults = joinBitmap(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), nextAttribute)
        })
        results
      }).toArray.toSet

  def joinRoaring(map: Map[Int, RoaringBitmap], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = activeRoaring(map, relations, nextAttribute)

      activeDomain.par.flatMap(value => {
        val filteredMap = filterRoaring(map, relations, nextAttribute, value)
        val partialResults = joinRoaring(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), nextAttribute)
        })
        results
      }).toArray.toSet

  def joinParallel(map: Map[Int, Set[Int]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = active(map, relations, nextAttribute)

      activeDomain.par.flatMap(value => {
        val filteredMap = filter(map, relations, nextAttribute, value)
        val partialResults = joinParallel(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), nextAttribute)
        })
        results
      }).toArray.toSet

   def join(query: Optimized):Set[Substitution] =
     val rows = query.rows
     val relations = query.predicates
     val attributes = query.variables
     join(rows, relations, attributes)

   def joinData(query: Optimized):Set[Substitution] =
     val rows = query.dataMap
     val relations = query.predicates
     val attributes = query.variables
     joinData(rows, relations, attributes)

   def joinBitmap(query: Optimized):Set[Substitution] =
     val rows = query.rowsBitmap
     val relations = query.predicates
     val attributes = query.variables
     joinBitmap(rows, relations, attributes)

   def joinRoaring(query: Optimized):Set[Substitution] =
     val rows = query.roaringBitmap
     val relations = query.predicates
     val attributes = query.variables
     joinRoaring(rows, relations, attributes)

   def joinParallel(query: Optimized):Set[Substitution] =
     val rows = query.rows
     val relations = query.predicates
     val attributes = query.variables
     joinParallel(rows, relations, attributes)
}
