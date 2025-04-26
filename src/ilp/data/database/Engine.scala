package ilp.data.database

import ilp.data.{Query, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import ilp.gpu.JoinManager
import org.roaringbitmap.RoaringBitmap

import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}

class Engine(val database: Database) {

  val dataIndex = database.getIndex()
  var cuda: Cuda = null

  initCuda()

  def initCuda(): this.type =
    val rows = Range(0, 10000).toArray
    val new_rows = Array.tabulate[Int](10, 10000)((i, j) => 0)
    cuda = new Cuda(rows, new_rows, database.bitsize);
    this

  def convert(bitmap: Array[Int]): Array[Int] = {
    val bits = new Array[Int](bitmap.length * 32)
    var i = 0
    while (i < bitmap.length) {
      for (b <- 0 until 32) {
        bits(i * 32 + (31 - b)) = (bitmap(i) >>> b) & 1
      }
      bits(i * 32) = 1
      i += 1
    }
    bits
  }

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
      }
      }.toMap

    newMap

  def filterData(dataMap: Map[Int, Set[Predicate]], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, Set[Predicate]] =
    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val crrData = dataMap(id)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          val newRows = crrData.filter(predicate => predicate.getVariable(indice) == value)
          id -> newRows
        else
          id -> crrData
      }
      }.toMap

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
      }
      }.toMap

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
      }
      }.toMap

    newMap

  def filterCudaBitmap(rowMap: Map[Int, Array[Int]], relations: Array[Predicate], attribute: Variable, values: Array[Variable]): Map[Int, Array[Array[Int]]] =

    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        val crrIndex = dataIndex(predicate.identifier())
        val crrRowBitmap = rowMap(id)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          val newRows = values.map(value => {
            crrIndex.getCudaRows(value, indice)
          })


          cuda.setRows(crrRowBitmap)
          cuda.setNewRows(newRows)
          cuda.init()

          JoinManager.runAny(newRows.length, database.bitsize, cuda)

          //cuda.runFlat()
          val result = cuda.getResult

          id -> result
        else
          id -> Array.fill[Array[Int]](values.length)(crrRowBitmap)
      }
      }.toMap

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

  def activeRoaring(rowMap: Map[Int, RoaringBitmap], tables: Array[Predicate], attribute: Variable): Set[Variable] =
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val id = predicate.identifier(index)
        val bitset = rowMap(id)
        dataIndex(predicate.identifier()).getValues(bitset, predicate.getIndex(attribute))
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)

  def activeCuda(rowMap: Map[Int, Array[Int]], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val id = predicate.identifier(index)
        val array = rowMap(id)
        dataIndex(predicate.identifier()).getValues(array, predicate.getIndex(attribute))
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

  def joinCudaBitmap(map: Map[Int, Array[Int]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = activeCuda(map, relations, nextAttribute).toArray
      if activeDomain.nonEmpty then

        val rowSet = filterCudaBitmap(map, relations, nextAttribute, activeDomain)
        activeDomain.zipWithIndex.flatMap { case (value, index) => {
          val filteredMap = rowSet.map { case (ii, rows) => {
            ii -> rows(index)
          }
          }

          val setMap = rowSet.map { case (ii, rows) => {
            ii -> dataIndex.head._2.convertFromBitmapToRows(rows(index))
          }}


          val partialResults = joinCudaBitmap(filteredMap, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), nextAttribute)
          })
          results
        }
        }.toArray.toSet
      else
        Set()

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

  def join(query: Optimized): Set[Substitution] =
    val rows = query.rows
    val relations = query.predicates
    val attributes = query.variables
    join(rows, relations, attributes)

  def joinData(query: Optimized): Set[Substitution] =
    val rows = query.dataMap
    val relations = query.predicates
    val attributes = query.variables
    joinData(rows, relations, attributes)

  def joinBitmap(query: Optimized): Set[Substitution] =
    val rows = query.rowsBitmap
    val relations = query.predicates
    val attributes = query.variables
    joinBitmap(rows, relations, attributes)

  def joinCuda(query: Optimized): Set[Substitution] =
    val rows = query.cudaBitmap
    val relations = query.predicates
    val attributes = query.variables
    joinCudaBitmap(rows, relations, attributes)

  def joinRoaring(query: Optimized): Set[Substitution] =
    val rows = query.roaringBitmap
    val relations = query.predicates
    val attributes = query.variables
    joinRoaring(rows, relations, attributes)

  def joinParallel(query: Optimized): Set[Substitution] =
    val rows = query.rows
    val relations = query.predicates
    val attributes = query.variables
    joinParallel(rows, relations, attributes)

}