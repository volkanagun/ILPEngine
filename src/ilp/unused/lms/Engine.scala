package ilp.lms

import ilp.data.Substitution
import ilp.data.database.{Database, Index, Optimized}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.concurrent.TrieMap as ConcurrentMap
import scala.collection.immutable.{BitSet, Set}
import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}


class Engine(val database: Database, val recursiveDepth: Int = 2) {

  val bitsize = database.bitsize
  var dataIndex = database.getIndex()

  def addIndex(predicate: Predicate, set: Set[Substitution]): this.type = {
    val id = predicate.identifier()
    val predicates = set.map(substitution => predicate.substitution(substitution).asPredicate())
    dataIndex = dataIndex.updated(id, dataIndex.getOrElse(id, Index(predicate, Array[Predicate](), bitsize)).addIndex(predicates))
    this
  }

  def cacheID(depth: Int, rule: Optimized, substitution: Substitution, nextAttribute: Variable): Int = {
    val items = Array(depth, rule.id(), nextAttribute.hashCode())
    items.foldRight(1){case(crr, main)=> main * 7 + crr}
  }

  def cacheHAS(cache: ConcurrentMap[Int, Set[Substitution]], id: Int): Boolean = {
    cache.synchronized{
      cache.contains(id)
    }
  }

  def cacheGET(cache: ConcurrentMap[Int, Set[Substitution]], id: Int): Set[Substitution] = {
    synchronized {
      cache(id)
    }
  }

  def cacheADD(cache: ConcurrentMap[Int, Set[Substitution]], id: Int, set: Set[Substitution]): Set[Substitution] = {
    synchronized {
      if set.nonEmpty then cache.put(id, set)
      set
    }
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
      .flatMap { case (predicate, index) => {
        val id = predicate.identifier(index)
        if dataMap.contains(id) then
          val crrData = dataMap(id)
          if predicate.contains(attribute) then
            val indice = predicate.getIndex(attribute)
            val newRows = crrData.filter(predicate => predicate.getVariable(indice).equalValue(value))
            Some(id -> newRows)
          else
            Some(id -> crrData)
        else
          None
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

  def activeCyclic(programMap: Map[Int, Array[Optimized]], crrSubstitution: Substitution,
                   crrQuery: Optimized,
                   filterMap: Map[Int, Set[Predicate]],
                   relations: Array[Predicate],
                   attributes: Array[Variable],
                   attribute: Variable,
                   crrDepth: Int): Set[Variable] = {
    val domains = crrQuery.getRelations().zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.isFunctional() && attribute.isSymbol() then
          Some(Set[Variable](attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then
          val predicateId = predicate.identifier()
          val id = predicate.identifier(index)
          val position = predicate.getIndex(attribute)

          val crrResults = filterMap.getOrElse(id, Set[Predicate]()).map(predicate => predicate.getVariable(position))
            .filter(variable => attribute.equalValue(variable))

          if crrResults.nonEmpty then
            Some(crrResults)
          else {
            val newRules = programMap.getOrElse(predicateId, Array[Optimized]())

            val otherResults = newRules.toSet.flatMap(newRule => {
              val newHead = newRule.getHead()
              val newVariable = newHead.getVariable(position)
              val newSubstitution = predicate.call(newHead, crrSubstitution)
                .composition(newVariable, attribute)
              val newAttributes = newRule.getVariables()
              val newRelations = newRule.getRelations()
              val newMap = newRule.getDataMap()

              val substitutions = joinCyclic(programMap, newSubstitution, newRule, newMap,
                newRelations,
                newAttributes,
                crrDepth + 1)
              substitutions.map(substitution => substitution.valueByVariable(newVariable, attribute.getName()).get)
            })

            Some(otherResults)
          }
        else
          None
      }
    }

    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def activeCyclicParallel(cache: ConcurrentMap[Int, Set[Substitution]], programMap: Map[Int, Array[Optimized]], crrSubstitution: Substitution,
                           crrQuery: Optimized,
                           filterMap: Map[Int, Set[Predicate]],
                           relations: Array[Predicate],
                           attributes: Array[Variable],
                           attribute: Variable,
                           crrDepth: Int): Set[Variable] = {
    val domains = crrQuery.getRelations().zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.isFunctional() && attribute.isSymbol() then
          Some(Set[Variable](attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then
          val predicateId = predicate.identifier()
          val id = predicate.identifier(index)
          val position = predicate.getIndex(attribute)

          val crrResults = filterMap.getOrElse(id, Set[Predicate]()).map(predicate => predicate.getVariable(position))
            .filter(variable => attribute.equalValue(variable))

          if crrResults.nonEmpty then
            Some(crrResults)
          else {
            val newRules = programMap.getOrElse(predicateId, Array[Optimized]())

            val otherResults = newRules.toSet.flatMap(newRule => {
              val newHead = newRule.getHead()
              val newVariable = newHead.getVariable(position)
              val newSubstitution = predicate.call(newHead, crrSubstitution)
                .composition(newVariable, attribute)
              val newAttributes = newRule.getVariables()
              val newRelations = newRule.getRelations()
              val newMap = newRule.getDataMap()

              val substitutions = joinCyclicParallel(cache, programMap, newSubstitution, newRule, newMap,
                newRelations,
                newAttributes,
                crrDepth + 1)
              substitutions.map(substitution => substitution.valueByVariable(newVariable, attribute.getName()).get)
            })

            Some(otherResults)
          }
        else
          None
      }
    }

    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }

  def activeBitmap(rowMap: Map[Int, BitSet], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.isFunctional() && attribute.isSymbol() then
          Some(Set(attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then {
          val predicateId = predicate.identifier()
          val rowId = predicate.identifier(index)
          val bitset = rowMap(rowId)
          val results = dataIndex(predicateId).getValues(bitset, predicate.getIndex(attribute))
            .filter(variable => attribute.equalValue(variable))

          Some(results)
        }
        else None
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }


  def activeRoaring(rowMap: Map[Int, RoaringBitmap], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
    val domains = tables.zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.isFunctional() && attribute.isSymbol() then
          Some(Set(attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then {
          val predicateId = predicate.identifier()
          val id = predicate.identifier(index)
          val bitset = rowMap(id)
          val results = dataIndex(predicateId).getValues(bitset, predicate.getIndex(attribute))
            .filter(variable => attribute.equalValue(variable))

          Some(results)
        }
        else None
      }
    }
    if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
  }


  def joinCyclic(programMap: Map[Int, Array[Optimized]], crrSubstitution: Substitution,
                 crrQuery: Optimized, dataMap: Map[Int, Set[Predicate]],
                 relations: Array[Predicate],
                 attributes: Array[Variable], crrDepth: Int = 0): Set[Substitution] = {
    if crrDepth > recursiveDepth then
      Set[Substitution]()
    else if attributes.isEmpty then Set(Substitution())
    else

      val newExecuteSubstitution = execute(crrQuery, crrSubstitution)
      val restAttributes = newExecuteSubstitution.compose(attributes.tail)
      val nextAttribute = newExecuteSubstitution.compose(attributes.head)

      val activeDomain = activeCyclic(programMap, crrSubstitution, crrQuery, dataMap, relations, restAttributes, nextAttribute, crrDepth)
      //val activeDomain = activeCyclic(programMap, newExecuteSubstitution, crrQuery, dataMap, relations, restAttributes, nextAttribute, crrDepth)

      activeDomain.flatMap(value => {
        val filteredMap = filterData(dataMap, relations, nextAttribute, value)
        val partialResults = joinCyclic(programMap, newExecuteSubstitution, crrQuery, filteredMap, relations, restAttributes, crrDepth)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
        })
        results
      }).toArray.toSet
  }

  def joinCyclicParallel(cache: ConcurrentMap[Int, Set[Substitution]], programMap: Map[Int, Array[Optimized]], crrSubstitution: Substitution,
                         crrQuery: Optimized, dataMap: Map[Int, Set[Predicate]],
                         relations: Array[Predicate],
                         attributes: Array[Variable], crrDepth: Int = 0): Set[Substitution] = {

    if crrDepth > recursiveDepth then
      Set[Substitution]()

    else if attributes.isEmpty then Set(Substitution())
    else

      val newExecuteSubstitution = execute(crrQuery, crrSubstitution)
      val restAttributes = newExecuteSubstitution.compose(attributes.tail)
      val nextAttribute = newExecuteSubstitution.compose(attributes.head)
      val cacheId = cacheID(0, crrQuery, newExecuteSubstitution, nextAttribute)
      if cacheHAS(cache, cacheId) then
        cacheGET(cache, cacheId)
      else
        val activeDomain = activeCyclicParallel(cache, programMap, newExecuteSubstitution, crrQuery, dataMap, relations, restAttributes, nextAttribute, crrDepth)
        val results = activeDomain.par.flatMap(value => {
          val filteredMap = filterData(dataMap, relations, nextAttribute, value)
          val partialResults = joinCyclicParallel(cache, programMap, newExecuteSubstitution, crrQuery, filteredMap, relations, restAttributes, crrDepth)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
          })
          results
        }).toArray.toSet

        cacheADD(cache, cacheId, results)

  }

  def joinBitmap(map: Map[Int, BitSet], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
    if attributes.isEmpty then Set(Substitution())
    else
      val nextAttribute = attributes.head
      val activeDomain = activeBitmap(map, relations, nextAttribute)

      activeDomain.par.flatMap(value => {
        val filteredMap = filterBitmap(map, relations, nextAttribute, value)
        val partialResults = joinBitmap(filteredMap, relations, attributes.tail)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), value)
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
          partial.appendNew(nextAttribute.toVariable(), value)
        })
        results
      }).toArray.toSet




  def joinCyclic(program: Array[Optimized], substitution: Substitution): Set[Substitution] =
    val programMap = program.groupBy(optimized => optimized.identifier())
    val rule = program.last
    val dataMap = rule.dataMap
    val attributes = rule.variables
    val relations = rule.predicates
    val result = joinCyclic(programMap, substitution, rule, dataMap, relations, attributes)
    result

  def joinCyclicParallel(program: Array[Optimized], substitution: Substitution): Set[Substitution] =
    val programMap = program.groupBy(optimized => optimized.identifier())
    val cache = ConcurrentMap[Int, Set[Substitution]]()
    val rule = program.last
    val dataMap = rule.dataMap
    val attributes = rule.variables
    val relations = rule.predicates
    val result = joinCyclicParallel(cache, programMap, substitution, rule, dataMap, relations, attributes)
    result

  def joinCyclicParallel(cache: ConcurrentMap[Int, Set[Substitution]], program: Array[Optimized], substitution: Substitution): Set[Substitution] =
    val programMap = program.groupBy(optimized => optimized.identifier())
    val rule = program.last
    val dataMap = rule.dataMap
    val attributes = rule.variables
    val relations = rule.predicates
    val result = joinCyclicParallel(cache, programMap, substitution, rule, dataMap, relations, attributes)
    result


  def execute(originalQuery: Optimized, substitution: Substitution = Substitution()): Substitution = {

    var main = substitution

    originalQuery.getQuery().getBody()
      .foreach(predicate => {
        val newPredicate = predicate.substitution(main)
          .asPredicate()
        if newPredicate.isExecutable() then {
          val newSubstitution = newPredicate.execute().get
          main = main.composition(newSubstitution)
        }

      })

    main
  }

  def joinBitmap(query: Optimized): Set[Substitution] =
    val rows = query.rowsBitmap
    val relations = query.predicates
    val attributes = query.variables
    joinBitmap(rows, relations, attributes)



  def joinRoaring(query: Optimized): Set[Substitution] =
    val rows = query.roaringBitmap
    val relations = query.predicates
    val attributes = query.variables
    joinRoaring(rows, relations, attributes)

  def joinRoaring(queries: Array[Optimized]): Set[Substitution] = {
    var set: Set[Substitution] = Set()
    for (queryOptimized <- queries) {
      val head = queryOptimized.getQuery().getHead()
      set = joinRoaring(queryOptimized)
      addIndex(head, set)
    }
    set
  }

}