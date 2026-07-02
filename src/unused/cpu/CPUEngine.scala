package ilp.cpu

import ilp.data.{Position, Query, Substitution}
import ilp.data.database.Database
import ilp.data.predicates.Predicate
import ilp.data.variables.{Sym, Variable}
import ilp.gpu.JoinManager

import java.util.jar.Attributes
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}
import scala.util.Random

case class Frame(
                  map: Map[Int, Array[Array[Int]]],
                  relations: Array[Predicate],
                  attributes: Set[Variable],
                  partialResults: Set[Substitution]
                )

class CPUEngine(database: Database, batchSize: Int = 1, localSize: Int = 1):
  //var tablesFlattened = Map[Int, Array[Int]]()
  var tables = Map[Int, Array[Array[Int]]]()

  var values = Map[Int, Set[Int]]()
  //var predicateMap = Map[Int, Array[Predicate]]()

  var colCount = Map[Int, Int]()
  var rowCount = Map[Int, Int]()

  var string2id = Map[String, Int]()
  var id2string = Map[Int, String]()

  var kernelMap = new KernelMap()

  def compile(): this.type =
    println("Compiling started ...")
    database.sets.foreach(predicate => {
      addTable(predicate)
    })
    println("Compilation finished...")
    this

  def compile(queries: Array[Query]): this.type =
    queries.foreach(query => compile(query))
    this

  def compile(query: Query): this.type =
    println("Compiling started ...")
    val identifiers = query.getBody().map(_.identifier())
    database.sets.filter(predicate => identifiers.contains(predicate.identifier())).foreach(predicate => {
      addTable(predicate)
    })
    println("Compilation finished...")
    this

  def getTables(): Map[Int, Array[Array[Int]]] = tables

  def addSymbolID(value: String): Int =
    if !string2id.contains(value) then
      val id = string2id.size
      string2id = string2id.updated(value, id)
      id2string = id2string.updated(id, value)
      id
    else
      string2id(value)

  def addSymbolID(value: Array[Sym]): Array[Int] =
    value.map(sym => addSymbolID(sym.value))

  def addValues(positions: Array[Position], symbols: Array[Int]): Unit = {
    val crrMap = positions.zip(symbols).foreach { case (position, value) => {
      val id = position.getValueIdentifier()
      val crrArr = values.getOrElse(id, Set[Int]()) + value
      values = values.updated(id, crrArr)
    }
    }
  }

  def updateColCount(id: Int, size: Int): Unit = {
    if !colCount.contains(id) then
      colCount = colCount.updated(id, size)
  }

  def incRowCount(id: Int): Unit = {
    if !rowCount.contains(id) then
      rowCount = rowCount.updated(id, 1)
    else
      rowCount = rowCount.updated(id, rowCount(id) + 1)
  }

  def addTable(predicate: Predicate): Unit = {
    val id = predicate.identifier()
    val positions = predicate.getPositions()
    val values = addSymbolID(predicate.getSymbols())

    addValues(positions, values)
    updateColCount(id, values.length)
    incRowCount(id)

    tables = tables.updated(id, tables.getOrElse(id, Array[Array[Int]]()) :+ values)
    //predicateMap = predicateMap.updated(id, predicateMap.getOrElse(id, Array[Predicate]()) :+ predicate)
  }

  def optimizeByDependency(query: Query): GPUQuery =
    val relations = query.getBody()
    val maxValue = string2id.size
    val gpuTables = relations.zipWithIndex.map { case (predicate, index) => {
      val id = predicate.identifier()
      val pid = predicate.identifier(index)
      val attributes = predicate.getVariables()
      val data = tables(id)
      GPUTable(predicate,  predicate.getName(), id, pid, attributes, data)
    }
    }

    val gpuQuery = GPUQuery(query.hashCode(),maxValue, gpuTables).init()
    val optimizedQuery = CPUQueryPlan(gpuQuery).optimizeByDependency()
    optimizedQuery

  def optimizeByDepth(query: Query): GPUQuery =
    val relations = query.getBody()
    val maxValue = string2id.size

    val gpuTables = relations.zipWithIndex.map { case (predicate, index) => {
      val id = predicate.identifier()
      val pid = predicate.identifier(index)
      val attributes = predicate.getVariables()
      val data = tables(id)
      GPUTable(predicate, predicate.getName(), id, pid, attributes, data)
    }
    }

    val gpuQuery = GPUQuery(query.hashCode(), maxValue, gpuTables).init()
    val optimizedQuery = CPUQueryPlan(gpuQuery).optimizeByDepth()
    optimizedQuery

  def optimizeByBranch(query: Query): GPUQuery =
    val relations = query.getBody()
    val maxValue = string2id.size
    val gpuTables = relations.zipWithIndex.map { case (predicate, index) => {
      val id = predicate.identifier()
      val pid = predicate.identifier(index)
      val attributes = predicate.getVariables()
      val data = tables(id)
      GPUTable(predicate, predicate.getName(), id, pid, attributes, data)
    }
    }

    val gpuQuery = GPUQuery(query.hashCode(),maxValue, gpuTables).init()
    val optimizedQuery = CPUQueryPlan(gpuQuery).optimizeByBranch()
    optimizedQuery

  def optimizeNone(query: Query): GPUQuery =
    val relations = query.getBody()
    val maxValue = string2id.size
    val gpuTables = relations.zipWithIndex.map { case (predicate, index) => {
      val id = predicate.identifier()
      val pid = predicate.identifier(index)
      val attributes = predicate.getVariables()
      val data = tables(id)
      GPUTable(predicate, predicate.getName(), id, pid, attributes, data)
    }
    }

    val gpuQuery = GPUQuery(query.hashCode(),maxValue, gpuTables).init()
    val optimizedQuery = CPUQueryPlan(gpuQuery).optimizeByNone()
    optimizedQuery

  def optimizeCPU(query: Query): GPUQuery =
    val relations = query.getBody()
    val maxValue = string2id.size
    val gpuTables = relations.zipWithIndex.map { case (predicate, index) => {
      val id = predicate.identifier()
      val pid = predicate.identifier(index)
      val attributes = predicate.getVariables()
      val data = tables(id)
      GPUTable(predicate, predicate.getName(), id, pid, attributes, data)
    }
    }

    val gpuQuery = GPUQuery(query.hashCode(),maxValue, gpuTables).init()
    val optimizedQuery = CPUQueryPlan(gpuQuery).optimizeByDepth()
    optimizedQuery

  def join(query: GPUQuery): Set[Substitution] = {
    val relations = query.getBody()
    val attributes = query.getAttributes()
    val items = relations.zipWithIndex.map { case (p, index) => {
      p.identifier(index) -> tables(p.identifier())
    }
    }.toMap

    join(items, relations, attributes)
  }

  def joinHeterogeneous(gpuQuery: GPUQuery): Set[Substitution] = {
    val crrTables = gpuQuery.tables
    val attributes = gpuQuery.getAttributes()
    val items = crrTables.map { table => {
      table.pid -> tables(table.id)
    }}.toMap

    joinHeterogeneous(items, gpuQuery, gpuQuery.getBody(), attributes)
  }

  def joinParallel(query: GPUQuery): Set[Substitution] = {
    val relations = query.getBody()
    val attributes = query.getAttributes() //relations.flatMap(predicate => predicate.getVariables()).toSet
    val items = relations.zipWithIndex.map { case (p, index) => {
      p.identifier(index) -> tables(p.identifier())
    }
    }.toMap

    joinParallel(items, relations, attributes)
  }


  def joinBatchParallel(optimizedQuery: GPUQuery): Set[Substitution] = {
    joinBatchParallel(optimizedQuery, optimizedQuery.getAttributes())
  }

  def joinBatchParallel(optimizedQueries: Array[GPUQuery]): Array[Set[Substitution]] = {
    optimizedQueries.par.map(optimizedQuery => joinBatchParallel(optimizedQuery, optimizedQuery.getAttributes()))
      .toArray
  }


  def active(map: Map[Int, Array[Array[Int]]], tables: Array[Predicate], attribute: Variable): Set[Int] = {
    val domains = tables.zipWithIndex.collect {
      case (predicate, index) if predicate.contains(attribute) => {
        val indice = predicate.getIndex(attribute)
        map(predicate.identifier(index)).map(row => row(indice)).toSet
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def activeGPU(map: Map[Int, Array[Array[Int]]], tables: Array[GPUTable], attribute: Variable): Set[Int] = {
    val domains = tables.collect {
      case predicate if predicate.contains(attribute) => {
        val indice = predicate.getIndex(attribute)
        map(predicate.pid).map(row => row(indice)).toSet
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def filter(map: Map[Int, Array[Array[Int]]], relations: Array[Predicate], attribute: Variable, value: Int): Map[Int, Array[Array[Int]]] =
    val newMap = relations.zipWithIndex
      .map { case (predicate, index) => {
        val id = predicate.identifier(index)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          id -> map(id).filter(row => row(indice) == value)
        else
          id -> map(id)
      }
      }.toMap

    newMap

  def filter(map: Map[Int, Array[Array[Int]]], query: GPUQuery, attribute: Variable, value: Int): GPUQuery =
    val rows = query.gpuRows()
    val newRows = query.tables.zipWithIndex
      .map { case (table, tableIndex) => {
        val id = table.pid
        val predicate = table.predicate
        val crrRows = rows(tableIndex)
        if predicate.contains(attribute) then
          val indice = predicate.getIndex(attribute)
          map(id).zipWithIndex.map { case (row, rowIndex) => {
            if crrRows(rowIndex) == 1 && row(indice) == value then
              1
            else
              0
          }}

        else
          crrRows
      }}

    query.newQuery(newRows)

  def filterResult(map: Map[Int, Array[Array[Int]]], result: Array[Array[Int]], predicates: Array[Int]): Map[Int, Array[Array[Int]]] =
    predicates.zipWithIndex.map { case (predicate, index) => {
      predicate -> result(index).zipWithIndex.filter { case (i, index) => i > 0 }
        .map { case (_, rowIndex) => map(predicate)(rowIndex) }

    }
    }.toMap

  def filterBatchResult(map: Map[Int, Array[Array[Int]]], result: Array[Array[Array[Int]]], predicates: Array[Int]): Array[Map[Int, Array[Array[Int]]]] =
    result.map(crrResult => {
      predicates.zipWithIndex.map { case (predicate, index) => {
        predicate -> crrResult(index).zipWithIndex.filter { case (i, index) => i > 0 }
          .map { case (_, rowIndex) => map(predicate)(rowIndex) }
      }
      }.toMap
    })


  def filterBaseGPU(regularTables: Map[Int, Array[Array[Int]]], relations: Array[Predicate], attribute: Variable, value: Int): Map[Int, Array[Array[Int]]] =

    val dataRelations = relations.zipWithIndex.map { case (predicate, index) => predicate.identifier(index) }
    val dataIndices = relations.zipWithIndex.filter { case (predicate, _) => predicate.contains(attribute) }
      .map { case (predicate, index) => (predicate.identifier(index), predicate.getIndex(attribute)) }
    val dataPositions = dataIndices.map(_._2)
    val dataPredicates = dataIndices.map(_._1)
    val dataTables = dataPredicates.map(regularTables)
    val dataRows = dataTables.map(_.length)
    val dataMaxRowSize = dataRows.max
    val kernel = new GPUFilter(dataTables, dataPositions, dataRows, dataMaxRowSize, value);
    JoinManager.runAny(dataTables.length, dataMaxRowSize, kernel)

    val newMap = filterResult(regularTables, kernel.results, dataPredicates)
    val finalMap = dataRelations.map(id => {
      id -> (if newMap.contains(id) then newMap(id) else regularTables(id))
    }).toMap
    finalMap

  def registerQuery(query: GPUQuery): this.type =
    println("Registering query")
    val dataTables = query.gpuTables()
    val dataPositions = Array.fill[Int](dataTables.length)(1)

    val dataRows = query.gpuRows()
    val rowSize = query.rowSize()
    val colSize = query.colSize()
    val rowMax = rowSize.max
    val valueMax = query.valueMax()
    val valueArray = Array.fill[Int](valueMax)(1)
    val batchKernel = new GPUBatchFilter(dataTables, dataRows, rowSize, colSize, dataPositions, valueArray, rowMax, localSize);
    val reducedKernel = new GPUIntersectFilter(dataTables, dataRows, rowSize, colSize, dataPositions, valueArray, rowMax, localSize);
    batchKernel.put(dataTables)
    batchKernel.put(rowSize)
    batchKernel.put(colSize)

    reducedKernel.put(dataTables)
    reducedKernel.put(rowSize)
    reducedKernel.put(colSize)
    kernelMap.register(query.id, batchKernel)

    /*

        query.getAttributeIds().foreach(id=>{
          val batchKernel = new GPUBatchFilter(dataTables, dataRows, rowSize, colSize, dataPositions, valueArray, rowMax, localSize);
          batchKernel.put(dataTables)
          batchKernel.put(rowSize)
          batchKernel.put(colSize)
          kernelMap.register(id, batchKernel)
        })
    */

    println("Registering finieshed...")
    this

  def disposeQuery(query: GPUQuery): this.type =
    kernelMap.get(query.id).dispose()
    this


  def filterBatchGPU(query: GPUQuery, attribute: Variable, values: Array[Int]): Array[GPUQuery] =

    var dataPositions = query.attr(attribute)
    var dataRows = query.gpuRows()
    var rowSize = query.rowSize()
    var colSize = query.colSize()
    val valueSize = values.size
    val valueArray = values

    if values.nonEmpty then
      val batchKernel = kernelMap.get(query.id)
      val pairPositions = dataPositions.zipWithIndex.filter(tuple=> tuple._1>=0).map(_._2)
      val pairFilter = dataRows.zip(rowSize).zip(colSize).zip(dataPositions).filter(tuple=> tuple._2 >= 0)
      val data = query.gpuData.zip(dataPositions).filter(tuple=> tuple._2>=0).map(_._1)
      //val batchKernel = new GPUBatchFilter(dataTables, dataRows, rowSize, colSize, dataPositions, valueArray, rowMax, localSize);
      dataPositions = pairFilter.map(_._2)
      colSize = pairFilter.map(_._1._2)
      rowSize = pairFilter.map(_._1._1._2)
      dataRows = pairFilter.map(_._1._1._1)

      val rowMax = rowSize.max

      batchKernel.setValueSize(valueArray.length)
      batchKernel.setValues(valueArray)
      batchKernel.setPositions(dataPositions)
      batchKernel.setRows(dataRows)
      batchKernel.setRowsize(rowSize)
      batchKernel.setTables(data)
      batchKernel.setColsize(colSize)
      batchKernel.init()

      JoinManager.runAny(rowMax, valueSize, dataPositions.length, batchKernel)
      //batchKernel.runFlat()
      val results = batchKernel.getResults()
      val finalResults = results.map(crrResult => {

        query.newQuery(pairPositions, crrResult)
      });

      finalResults
    else
      Array()


  def filterParallelBatchGPU(query: GPUQuery, attribute: Variable, values: Array[Int]): Array[GPUQuery] =

    val dataPositions = query.attr(attribute)
    val dataTables = query.gpuTables()
    val dataRows = query.gpuRows()
    val rowSize = query.rowSize()
    val colSize = query.colSize()
    val rowMax = rowSize.max
    val valueSize = values.size
    val valueArray = values


    if values.nonEmpty then
      val myKernel = kernelMap.get(query.id);
      myKernel.setValueSize(valueArray.length)
      myKernel.setValues(valueArray)
      myKernel.setPositions(dataPositions)
      myKernel.setRows(dataRows)
      myKernel.setValueSize(valueArray.length)
      myKernel.init()
      JoinManager.runAny(rowMax, valueSize, dataTables.length, myKernel)

      val results = myKernel.getResults()
      val finalResults = results.map(crrResult => {
        query.newQuery(crrResult)
      });

      finalResults
    else
      Array()


  def join(map: Map[Int, Array[Array[Int]]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = active(map, relations, nextAttribute)

      activeDomain.flatMap(value => {

        val filteredMap = filter(map, relations, nextAttribute, value)
        val partialResults = join(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
        })
        results
      })

  def joinParallel(map: Map[Int, Array[Array[Int]]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = active(map, relations, nextAttribute)
      val debug = 0;
      activeDomain.par.flatMap(value => {
        val filteredMap = filter(map, relations, nextAttribute, value)
        val partialResults = joinParallel(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
        })
        results
      }).toArray.toSet

  def joinHeterogeneous(map: Map[Int, Array[Array[Int]]], gpuQuery: GPUQuery, relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then {
      Set(Substitution())
    }
    else
      val nextAttribute = attributes.head

      val activeDomain = gpuQuery.getActive(nextAttribute).toArray
      val d = 0
      if activeDomain.size > 50 then
        val newQuery = filterParallelBatchGPU(gpuQuery, nextAttribute, activeDomain)
        newQuery.zip(activeDomain).flatMap { case (crrQuery, value) => {
          val partialResults = joinHeterogeneous(map, crrQuery, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
          })
          results
        }
        }.toSet

      else
        activeDomain.flatMap(value => {
          val newQuery = filter(map, gpuQuery, nextAttribute, value)
          val partialResults = joinHeterogeneous(map, newQuery, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
          })
          results
        }).toSet

  def joinBaseGPU(map: Map[Int, Array[Array[Int]]], relations: Array[Predicate], attributes: Set[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = active(map, relations, nextAttribute)

      val shuffle = activeDomain.toArray.sorted
      var finalSet = Set[Substitution]()
      shuffle.foreach(value => {

        val filteredMap = filterBaseGPU(map, relations, nextAttribute, value)
        val partialResults = joinBaseGPU(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
        })
        finalSet ++= results
      })
      finalSet

  def joinBaseStackGPU(map: Map[Int, Array[Array[Int]]], relations: Array[Predicate], attributes: Set[Variable]): Set[Substitution] = {
    if (attributes.isEmpty) return Set(Substitution())

    val stack = mutable.Stack[Frame]()
    var finalSet = Set[Substitution]()

    // Push the initial frame onto the stack
    stack.push(Frame(map, relations, attributes, Set(Substitution())))

    while (stack.nonEmpty) {
      val currentFrame = stack.pop()
      val currentMap = currentFrame.map
      val currentTables = currentFrame.relations
      val currentAttributes = currentFrame.attributes
      val currentPartialResults = currentFrame.partialResults

      if (currentAttributes.isEmpty) {
        // If no attributes are left, add the partial results to the final set
        finalSet ++= currentPartialResults
      } else {
        // Process the next attribute
        val nextAttribute = currentAttributes.head
        val remainingAttributes = currentAttributes.tail
        val activeDomain = active(currentMap, currentTables, nextAttribute)
        val shuffle = activeDomain.toArray.sorted
        shuffle.foreach { value =>
          val filteredMap = filterBaseGPU(currentMap, currentTables, nextAttribute, value)
          val newPartialResults = currentPartialResults.map { partial =>
            partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
          }
          stack.push(Frame(filteredMap, currentTables, remainingAttributes, newPartialResults))
        }
      }
    }

    finalSet
  }

  def joinBatchParallel(gpuQuery: GPUQuery, attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then {
      System.gc()
      Set(Substitution())
    }
    else
      val nextAttribute = attributes.head
      val remainingAttributes = attributes.tail
      val activeDomain = gpuQuery.getActive(nextAttribute).toArray
      //println("Active size: "+activeDomain.size)
      activeDomain.sliding(batchSize, batchSize).toArray.flatMap(activeBatch => {
        val newQuery = filterBatchGPU(gpuQuery, nextAttribute, activeBatch)
        newQuery.zip(activeBatch).flatMap { case (crrQuery, value) => {
          val partialResults = joinBatchParallel(crrQuery, remainingAttributes)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute, nextAttribute.toSymbol(id2string(value)))
          })
          results
        }
        }.toSet
      }).toSet



