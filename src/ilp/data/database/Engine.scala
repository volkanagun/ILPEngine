package ilp.data.database

import ilp.data.{Hypothesis, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.collection.concurrent.TrieMap as ConcurrentMap
import scala.collection.immutable.{BitSet, Set}
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable


class Engine(val database: Database, val recursiveDepth: Int = 5) {

  var globalCache = ConcurrentMap[Int, Set[Substitution]]()

  def getDatabase() = database

  def validHypothesis(hypothesis: Hypothesis): Boolean =
    database.getBias().getHypothesis(hypothesis).isDefined

  def atomSubstitutions(headPredicate: Predicate, substitution: Substitution): Set[Substitution] = {
    val substitutions = database.getSubstitutions(headPredicate)
    substitutions.filter(crrSubstitution => !crrSubstitution.conflicts(substitution))
  }

  def cacheID(depth: Int, rule: Optimized, substitution: Substitution, nextAttribute: Variable): Int = {
    val items = Array(depth, rule.getQueryId(), nextAttribute.hashCode())
    items.foldRight(1) { case (crr, main) => main * 7 + crr }
  }

  def cacheHAS(cache: ConcurrentMap[Int, Set[Substitution]], id: Int): Boolean = {
    cache.synchronized {
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
      cache.put(id, set)
      set
    }
  }

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

  def intersection(domains: Array[Set[Variable]]): Set[Variable] =
    if domains.isEmpty then {
      Set.empty
    }
    else {
      domains.reduce(_ intersect _)
    }

  def filterData(dataMap: Map[Int, Set[Predicate]], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, Set[Predicate]] =
    val newMap = relations.zipWithIndex
      .flatMap { case (predicate, index) => {
        val id = predicate.identifier(index)
        if dataMap.contains(id) then
          val crrData = dataMap(id)
          if predicate.contains(attribute) then
            val indice = predicate.getPosition(attribute)
            val newPredicates = crrData.filter(predicate => predicate.getVariable(indice).equalValue(value))
            Some(id -> newPredicates)
          else
            Some(id -> crrData)
        else
          None
      }
      }.toMap

    newMap

  def filterRoaring(rowMap: Map[Int, RoaringBitmap], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, RoaringBitmap] =
    val newMap = relations.zipWithIndex
      .flatMap { case (predicate, position) => {
        val pid = predicate.identifier(position)
        if rowMap.contains(pid) then
          val crrRows = rowMap(pid)
          if predicate.contains(attribute) then
            val predicateId = predicate.identifier()
            val crrIndex = database.getIndex(predicateId)
            val indice = predicate.getPosition(attribute)
            val newRows = crrIndex.getHavingRows(crrRows, value, indice)
            Some(pid -> newRows)
          else
            Some(pid -> crrRows)
        else
          None
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
          val position = predicate.getPosition(attribute)

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

  def activeCyclic(programMap: Map[Int, Array[Optimized]], context: ContextData,
                   attribute: Variable): Set[Variable] = {
    val domains = context.getRelations().zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.isFunctional() && attribute.isSymbol() then
          Some(Set[Variable](attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then
          val predicateId = predicate.identifier()
          val id = predicate.identifier(index)
          val position = predicate.getPosition(attribute)

          val crrResults = context.getDataMap().getOrElse(id, Set[Predicate]()).map(predicate => predicate.getVariable(position))
            .filter(variable => attribute.equalValue(variable))

          if crrResults.nonEmpty then
            Some(crrResults)
          else {
            val newRules = programMap.getOrElse(predicateId, Array[Optimized]())

            val otherResults = newRules.toSet.flatMap(newRule => {
              val newContext = ContextData(newRule, predicate, context.getSubstitution(), attribute, position, context.getDepth() + 1)
              val newVariable = newContext.getTargetVariable()
              val substitutions = joinCyclic(programMap, newContext)
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

  def activeParallel(contextMap: Map[Int, Array[ContextData]],
                     programContext: ContextData,
                     nextContext: ContextData): Set[Variable] = {

    val dataMap = nextContext.getDataMap()
    val attribute = nextContext.getTargetVariable()

    val domains = nextContext.getRelations().zipWithIndex.flatMap {
      case (predicate, index) => {

        if predicate.isFunctional() && predicate.contains(attribute) && attribute.isSymbol() then

          Some(Set[Variable](attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then
          val predicateId = predicate.identifier()
          val id = predicate.identifier(index)
          val position = predicate.getPosition(attribute)
          val crrResults = dataMap.getOrElse(id, Set[Predicate]()).map(predicate => predicate.getVariable(position).setName(attribute.getName()))
            .filter(variable => attribute.equalValue(variable))

          if crrResults.isEmpty then

            val results = contextMap(predicateId)
              .flatMap(currentContext => {
                currentContext.switchContext(nextContext.getSubstitution(), predicate, position, nextContext.getDepth() + 1)
              }).flatMap(currentContext => {
                val substitutions = joinParallel(contextMap, programContext, currentContext)
                val newVariable = currentContext.getHead().getVariable(position)
                if newVariable.getName() == "X1" then
                  val debug = 0;
                val newValues = substitutions.flatMap(substitution => substitution.valueByVariable(newVariable, attribute.getName())
                  .map(variable => variable))
                newValues
              }).toSet

            if results.nonEmpty then Some(results)
            else None
          else
            Some(crrResults)
        else
          None
      }
    }

    intersection(domains)
  }


  def activeCyclicRoaring(contextMap: Map[Int, Array[ContextRows]], context: ContextRows): Set[Variable] = {

    val rowMap = context.getRowMap()
    val attribute = context.getTargetVariable()
    val newSubstitution = context.getSubstitution()

    val domains = context.getRelations().zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.isFunctional() && attribute.isSymbol() then
          Some(Set[Variable](attribute))
        else if !predicate.isFunctional() && predicate.contains(attribute) then {
          val predicateId = predicate.identifier()
          val pid = predicate.identifier(index)
          val bitset = rowMap.getOrElse(pid, RoaringBitmap())
          val position = predicate.getPosition(attribute)

          val dataValues = database.getValues(predicateId, position, bitset)
          val crrResults = dataValues.filter(variable => attribute.equalValue(variable))

          if crrResults.isEmpty then
            val newContexts = contextMap(predicateId).map(contextRow => contextRow
              .switchContext(newSubstitution, predicate, context.depth + 1))

            val results = newContexts.flatMap(newContext => {
              val substitutions = joinRoaringParallel(contextMap, newContext)
              val newVariable = newContext.getTargetVariable()
              substitutions.flatMap(substitution => substitution.valueByVariable(newVariable, attribute.getName()))
            }).toSet

            Some(results)
          else
            Some(crrResults)
        }
        else
          None
      }
    }

    intersection(domains)
  }
  /*

    def activeCyclicRoaring(contextMap: Map[Int, Array[ContextRows]], context: ContextRows,
                            newSubstitution: Substitution,
                            attribute: Variable): Set[Variable] = {

      val rowMap = context.getRowMap()

      val domains = context.getRelations().zipWithIndex.flatMap {
        case (predicate, index) => {
          if predicate.isFunctional() && attribute.isSymbol() then
            Some(Set[Variable](attribute))
          else if !predicate.isFunctional() && predicate.contains(attribute) then {
            val predicateId = predicate.identifier()
            val pid = predicate.identifier(index)
            val bitset = rowMap(pid)
            val position = predicate.getPosition(attribute)
            val dataValues = database.getValues(predicateId, position, bitset)
            val crrResults = dataValues.filter(variable => attribute.equalValue(variable))

            if crrResults.isEmpty then
              val newContexts = contextMap(predicateId).map(contextRow => contextRow
                .newContext(newSubstitution, context.depth + 1))

              val results = newContexts.flatMap(newContext => {
                val substitutions = joinRoaringParallel(contextMap, newContext)
                val newVariable = newContext.getTargetVariable()
                substitutions.map(substitution => substitution.valueByVariable(newVariable, attribute.getName()).get)
              }).toSet

              Some(results)
            else
              Some(crrResults)
          }
          else
            None
        }
      }

      intersection(domains)
    }
  */

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

      val activeDomain = activeCyclic(programMap, newExecuteSubstitution, crrQuery, dataMap, relations, restAttributes, nextAttribute, crrDepth)

      activeDomain.flatMap(value => {
        val filteredMap = filterData(dataMap, relations, nextAttribute, value)
        val partialResults = joinCyclic(programMap, newExecuteSubstitution, crrQuery, filteredMap, relations, restAttributes, crrDepth)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
        })
        results
      }).toArray.toSet
  }

  def joinCyclic(programMap: Map[Int, Array[Optimized]], context: ContextData): Set[Substitution] = {
    if context.getDepth() > recursiveDepth then
      Set[Substitution]()
    else if context.emptyAttributes() then Set(Substitution())
    else

      val attributes = context.getAttributes()
      val newExecuteSubstitution = execute(context.getRule(), context.getSubstitution())
      val restAttributes = newExecuteSubstitution.compose(attributes.tail)
      val nextAttribute = newExecuteSubstitution.compose(attributes.head)
      val newContext = context.newContext(nextAttribute, restAttributes)
      val activeDomain = activeCyclic(programMap, newContext, nextAttribute)

      activeDomain.flatMap(value => {
        val filteredMap = filterData(context.getDataMap(), context.getRelations(), nextAttribute, value)
        val partialResults = joinCyclic(programMap, newContext)
        val results = partialResults.map(partial => {
          partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
        })
        results
      }).toArray.toSet
  }

  def joinParallel(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

    val contextProgram = programs.map(rule => ContextData(rule, Substitution()))
    val contextMap = contextProgram
      .groupBy { context => context.getHead().identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      if context.isTarget() then context.setSubstitution(substitution)

      val headPredicate = context.getHead()
      val crrSubstitutions = joinParallel(contextMap, context, context)
        .map(substitution => substitution.get(headPredicate.getVariables())) ++ atomSubstitutions(headPredicate, substitution)
      val crrPredicates = context.get(crrSubstitutions)
      substitutions = substitutions ++ (if context.isTarget() then crrSubstitutions else Set())
      contextProgram.filter(other => context.calledFrom(other))
        .foreach(other => other.updateData(headPredicate, crrPredicates))
    })

    substitutions
  }

  def joinSerial(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

    val contextProgram = programs.map(rule => ContextData(rule, substitution))
    val contextMap = contextProgram
      .groupBy { context => context.getHead().identifier() }

    var substitutions = Set[Substitution]()

    contextProgram.foreach(context => {

      val headPredicate = context.getHead()
      val crrSubstitutions = joinSerial(contextMap, context, context)
        .map(substitution => substitution.get(headPredicate.getVariables())) ++ atomSubstitutions(headPredicate, substitution)
      val crrPredicates = context.get(crrSubstitutions)
      substitutions = substitutions ++ (if context.isTarget() then crrSubstitutions else Set())
      contextProgram.filter(other => context.calledFrom(other))
        .foreach(other => other.updateData(headPredicate, crrPredicates))
    })

    substitutions
  }

  def joinRoaringParallel(programs: Array[Optimized], substitution: Substitution): Set[Substitution] = {

    val contextOrder = programs.map(rule => ContextRows(rule, substitution))
    val contextMap = contextOrder.groupBy(context => context.getHead().identifier())


    val map = programs.groupBy(optimized => optimized.identifier())
    var substitutions = Set[Substitution]()
    contextOrder.foreach(context => {
      val headPredicate = context.getHead()
      val crrSubstitutions = joinRoaringParallel(contextMap, context)
        .map(substitution => substitution.get(headPredicate.getVariables())) ++ atomSubstitutions(headPredicate, substitution)
      val crrPredicates = context.get(crrSubstitutions)

      database.index(headPredicate, crrPredicates)

      substitutions = substitutions ++ (if context.isTarget() then crrSubstitutions else Set())
      contextOrder.filter(other => context.calledFrom(other))
        .foreach(other => other.updateData(context.getHead(), crrPredicates))
    })

    substitutions
  }

  def joinParallelCache(programs: Array[Optimized], substitution: Substitution): Set[Substitution] = {

    val contextMap = programs.map(rule => ContextData(rule, substitution))
      .groupBy(context => context.getHead().identifier())

    var substitutions = Set[Substitution]()
    contextMap.foreach { case (id, contexts) => {

      contexts.foreach(context => {
        val headPredicate = context.getHead()
        val ruleId = context.getRuleId()
        val crrSubstitutions = if cacheHAS(globalCache, ruleId) then {
          cacheGET(globalCache, ruleId)
        }
        else {
          val substitutions = joinParallel(contextMap, context, context)
            .map(substitution => substitution.get(headPredicate.getVariables())) ++
            atomSubstitutions(headPredicate, substitution)

          cacheADD(globalCache, ruleId, substitutions)
        }

        substitutions = substitutions ++ (if context.isTarget() then crrSubstitutions else Set())
        contexts.filter(other => context.calledFrom(other))
          .foreach(other => other.updateData(context.getHead(), context.get(crrSubstitutions)))
      })
    }
    }

    substitutions
  }
  /*
    def joinCyclicBottomUp(programs: Array[Optimized], substitution: Substitution): Set[Substitution] = {
      //create dataMap for missing values
      val contexts = programs.map(rule => ContextData(rule, substitution))
      val map = programs.groupBy(optimized => optimized.identifier())
      var substitutions = Set[Substitution]()
      contexts.foreach(context => {
        substitutions = joinBottomUp(map, context)
        substitutions = substitutions.filter(substitution => substitution.containsAll(context.getHeadVariables()))
        val predicates = context.get(substitutions)
        contexts.filter(other => context.calledFrom(other))
          .foreach(other => other.updateData(context.getHead(), predicates))
      })
      substitutions
    }*/

  def joinSerial(contextMap: Map[Int, Array[ContextData]], programContext: ContextData, context: ContextData): Set[Substitution] = {
    if (context.getDepth() > recursiveDepth || context.emptyAttributes()) && execute(context).isDefined then
      Set(Substitution())
    else
      val newExecuteResult = execute(context)
      if newExecuteResult.isEmpty then Set()
      else {
        val newSubstitution = newExecuteResult.get
        val nextContext = context.nextContext(newSubstitution)
        val nextVariable = nextContext.getTargetVariable()
        val activeDomain = activeParallel(contextMap, programContext, nextContext)

        val results = activeDomain.flatMap(value => {
          val filteredMap = filterData(nextContext.getDataMap(), nextContext.getRelations(), nextContext.getTargetVariable(), value)
          val newContext = nextContext.newContext(newSubstitution.composition(value))
            .setDataMap(filteredMap)

          val partialResults = joinSerial(contextMap, programContext, newContext)
          val substitutions = partialResults.map(partial => {
            partial.appendNew(nextVariable.toVariable(), value.setName(nextVariable.getName()))
          })
          substitutions
        }).toArray.toSet

        results

      }

  }

  def joinParallel(contextMap: Map[Int, Array[ContextData]], programContext: ContextData, currentContext: ContextData): Set[Substitution] = {

    val executedContext = execute(currentContext)


    if executedContext.isEmpty then Set()
    //else if currentContext.emptyAttributes() && programContext.conflictContext(currentContext) then Set()
    else if (currentContext.getDepth() > recursiveDepth || currentContext.emptyAttributes()) && executedContext.isDefined then
      Set(Substitution())
    else {
      val newSubstitution = executedContext.get
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable()
      val activeDomain = activeParallel(contextMap, programContext, nextContext)

      if activeDomain.size == 0 then
        val de = 0;

      val results = activeDomain.flatMap(value => {
        val filteredMap = filterData(currentContext.getDataMap(), currentContext.getRelations(), nextVariable, value)

        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setDataMap(filteredMap)

        if (value.getName() == "Y2" || value.getName() == "X2") && newContext.getHead().getName().startsWith("f") then
          val debug = 0

        val partialResults = joinParallel(contextMap, programContext, newContext)
        val substitutions = partialResults.map(partial => {
          partial.replaceNew(nextVariable, value.setName(nextVariable.getName()))
        })
        substitutions
      }).toArray.toSet

      results
    }


  }

  def joinExecution(context: ContextRows): Option[ContextRows] =
    val newExecuteSubstitution = execute(context)
    if newExecuteSubstitution.isEmpty then None
    else if context.getDepth() > recursiveDepth || context.emptyAttributes() then Some(context)
    else {
      val executionSubstitution = newExecuteSubstitution.get
      val attributes = context.getAttributes()
      val restAttributes = executionSubstitution.compose(attributes.tail)
      val nextAttribute = executionSubstitution.compose(attributes.head)
      Some(context.newContext(executionSubstitution, nextAttribute, restAttributes))
    }

  def joinRoaringParallel(contextMap: Map[Int, Array[ContextRows]], context: ContextRows): Set[Substitution] = {
    val executedContext = joinExecution(context)
    if executedContext.isEmpty then Set()
    else if (context.getDepth() > recursiveDepth || context.emptyAttributes()) then
      Set(Substitution())
    else
      val activeDomain = activeCyclicRoaring(contextMap, executedContext.get)

      val results = activeDomain.par.flatMap(value => {
          val filteredRowMap = filterRoaring(context.getRowMap(), context.getRelations(), context.getTargetVariable(), value)
          executedContext.toSet.flatMap(newContext => {
            val nextAttribute = newContext.getTargetVariable()
            val newRowContext = newContext.newContext(value)
              .setRowMap(filteredRowMap)
            val partialResults = joinRoaringParallel(contextMap, newRowContext)
            val substitutions = partialResults.map(partial => {
              partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
            })
            substitutions
          })
        }).toArray
        .toSet

      results
  }
  /*
    def joinBottomUp(programMap: Map[Int, Array[Optimized]], context: ContextData): Set[Substitution] = {

      if context.emptyAttributes() then Set(Substitution())
      else
        val newExecuteSubstitution = executeConflict(context).get
        val attributes = context.getAttributes()
        val restAttributes = newExecuteSubstitution.compose(attributes.tail)
        val nextAttribute = newExecuteSubstitution.compose(attributes.head)
        val newContext = context.newContext(newExecuteSubstitution, nextAttribute, restAttributes)
        val activeDomain = activeCyclic(programMap, context, nextAttribute)
        val results = activeDomain.flatMap(value => {
          val filteredMap = filterData(context.getDataMap(), context.getRelations(), nextAttribute, value)
          newContext.setDataMap(filteredMap)
          val partialResults = joinParallel(Map(), newContext)
          val substitutions = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
          })
          substitutions
        }).toArray.toSet

        results

    }*/
  /*
    def joinCyclicBitmap(cache: ConcurrentMap[Int, Set[Substitution]], programMap: Map[Int, Array[Optimized]], crrSubstitution: Substitution,
                         crrQuery: Optimized, bitmapMap: Map[Int, BitSet],
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
          val activeDomain = activeCyclicBitmap(cache, programMap, newExecuteSubstitution, crrQuery, bitmapMap, relations, restAttributes, nextAttribute, crrDepth)
          val results = activeDomain.par.flatMap(value => {
            val filteredMap = filterBitmap(bitmapMap, relations, nextAttribute, value)
            val partialResults = joinCyclicBitmap(cache, programMap, newExecuteSubstitution, crrQuery, filteredMap, relations, restAttributes, crrDepth)
            val results = partialResults.map(partial => {
              partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
            })
            results
          }).toArray.toSet

          cacheADD(cache, cacheId, results)
    }*/
  /*

    def joinCyclicRoaring(cache: ConcurrentMap[Int, Set[Substitution]], programMap: Map[Int, Array[Optimized]], crrSubstitution: Substitution,
                          crrQuery: Optimized, bitmapMap: Map[Int, RoaringBitmap],
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
          val activeDomain = activeCyclicRoaring(cache, programMap, newExecuteSubstitution, crrQuery, bitmapMap, relations, restAttributes, nextAttribute, crrDepth)
          val results = activeDomain.par.flatMap(value => {
            val filteredMap = filterRoaring(bitmapMap, relations, nextAttribute, value)
            val partialResults = joinCyclicRoaring(cache, programMap, newExecuteSubstitution, crrQuery, filteredMap, relations, restAttributes, crrDepth)
            val results = partialResults.map(partial => {
              partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
            })
            results
          }).toArray.toSet

          cacheADD(cache, cacheId, results)

    }
  */


  def joinCyclic(program: Array[Optimized], substitution: Substitution): Set[Substitution] =
    val programMap = program.groupBy(optimized => optimized.identifier())
    val rule = program.last
    val dataMap = rule.dataMap
    val attributes = rule.variables
    val relations = rule.predicates
    val result = joinCyclic(programMap, substitution, rule, dataMap, relations, attributes)
    result

  /*  def joinCyclicRoaring(program: Array[Optimized], substitution: Substitution): Set[Substitution] =
      val programMap = program.groupBy(optimized => optimized.identifier())
      val cache = ConcurrentMap[Int, Set[Substitution]]()
      val rule = program.last
      val dataMap = rule.getRoaringMap()
      val attributes = rule.variables
      val relations = rule.predicates
      val result = joinCyclicRoaring(cache, programMap, substitution, rule, dataMap, relations, attributes)
      result*/

  /*  def joinCyclicBitmap(program: Array[Optimized], substitution: Substitution): Set[Substitution] =
      val programMap = program.groupBy(optimized => optimized.identifier())
      val cache = ConcurrentMap[Int, Set[Substitution]]()
      val rule = program.last
      val dataMap = rule.getBitmapMap()
      val attributes = rule.variables
      val relations = rule.predicates
      val result = joinCyclicBitmap(cache, programMap, substitution, rule, dataMap, relations, attributes)
      result*/

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


  def execute(context: ContextData): Option[Substitution] = {
    val rule = context.getRule()
    var main = context.getSubstitution()

    rule.getQuery().getBody()
      .foreach(predicate => {
        val newPredicate = predicate.substitution(main)
          .asPredicate()
        if newPredicate.isFunctional() && newPredicate.isDefinite() then {
          if newPredicate.isExecutable() then {
            newPredicate.execute().foreach(newSubstitution => {
              val newComposed = main.composition(newSubstitution)
              if main.hasConflict(newComposed) then {
                return None
              }
              else {
                main = newComposed
              }
            })
          }
          else {
            return None
          }
        }
      })

    Some(main)
  }

  def execute(context: ContextRows): Option[Substitution] = {
    val rule = context.getRule()
    var main = context.getSubstitution()


    rule.getQuery().getBody()
      .foreach(predicate => {
        val newPredicate = predicate.substitution(main)
          .asPredicate()
        if newPredicate.isFunctional() && newPredicate.isDefinite() then {
          if newPredicate.isExecutable() then {
            newPredicate.execute().foreach(newSubstitution => {
              val newComposed = main.composition(newSubstitution)
              if main.hasConflict(newComposed) then {
                return None
              }
              else {
                main = newComposed
              }
            })
          }
          else {
            return None
          }
        }
      })

    Some(main)
  }
  /*

    def execute(context: ContextRows): Substitution = {
      val rule = context.getRule()
      var main = context.getSubstitution()

      rule.getQuery().getBody()
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
  */

  /*

    def active(rowMap: Map[Int, Set[Int]], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
      val domains = tables.zipWithIndex.flatMap {
        case (predicate, index) => {
          if predicate.isFunctional() && attribute.isSymbol() then
            Some(Set(attribute))
          else if !predicate.isFunctional() && predicate.contains(attribute) then
            val predicateId = predicate.identifier()
            val rowId = predicate.identifier(index)
            val indice = predicate.getIndex(attribute)
            //println(predicate.name + ":" + rowMap.contains(rowId) + ":" + dataIndex.contains(predicateId))
            val rows = rowMap(rowId)
            val results = dataIndex(predicateId).getValues(rows, predicate.getIndex(attribute))
              .filter(variable => attribute.equalValue(variable))
            Some(results)
          else None
        }
      }

      if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
    }
  */
  /*

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
  */
  /*

    def activeSymbol(dataMap: Map[Int, Set[Predicate]], tables: Array[Predicate], attribute: Variable): Set[Variable] = {
      val domains = tables.zipWithIndex.flatMap {
        case (predicate, index) => {
          if predicate.isFunctional() && attribute.isSymbol() then
            Some(Set(attribute))
          else if !predicate.isFunctional() && predicate.contains(attribute) then
            val id = predicate.identifier(index)
            val indice = predicate.getIndex(attribute)
            val results = dataMap(id).map(predicate => predicate.getVariable(indice))
              .filter(variable => attribute.equalValue(variable))
            Some(results)
          else None
        }
      }

      if (domains.isEmpty) Set.empty else domains.reduce(_ intersect _)
    }

  */

  /*
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
  */

  /*

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
  */
  /*

    def join(map: Map[Int, Set[Int]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
      if attributes.isEmpty then Set(Substitution())
      else
        val nextAttribute = attributes.head
        val activeDomain = active(map, relations, nextAttribute)

        activeDomain.flatMap(value => {
          val filteredMap = filter(map, relations, nextAttribute, value)
          val partialResults = join(filteredMap, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), value)
          })
          results
        })
  */
  /*

    def joinData(map: Map[Int, Set[Predicate]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
      if attributes.isEmpty then Set(Substitution())
      else
        val nextAttribute = attributes.head
        val activeDomain = activeData(map, relations, nextAttribute)

        activeDomain.par.flatMap(value => {
          val filteredMap = filterData(map, relations, nextAttribute, value)
          val partialResults = joinData(filteredMap, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), value)
          })
          results
        }).toArray.toSet
  */
  /*

    def joinSymbolData(map: Map[Int, Set[Predicate]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
      if attributes.isEmpty then Set(Substitution())
      else
        val nextAttribute = attributes.head
        val activeDomain = activeSymbol(map, relations, nextAttribute)

        activeDomain.flatMap(value => {
          val filteredMap = filterData(map, relations, nextAttribute, value)
          val partialResults = joinSymbolData(filteredMap, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), value.setName(nextAttribute.getName()))
          })
          results
        }).toArray.toSet
  */

  /*
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
        }).toArray.toSet*/
  /*

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
  */

  /*

    def joinParallel(map: Map[Int, Set[Int]], relations: Array[Predicate], attributes: Array[Variable]): Set[Substitution] =
      if attributes.isEmpty then Set(Substitution())
      else
        val nextAttribute = attributes.head
        val activeDomain = active(map, relations, nextAttribute)

        activeDomain.par.flatMap(value => {
          val filteredMap = filter(map, relations, nextAttribute, value)
          val partialResults = joinParallel(filteredMap, relations, attributes.tail)
          val results = partialResults.map(partial => {
            partial.appendNew(nextAttribute.toVariable(), value)
          })
          results
        }).toArray.toSet
  */

  /*

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
  */
  /*

    def joinSymbolExecute(query: Optimized): Set[Substitution] =
      val executedQuery = execute(query, Substitution())
      val result = joinSymbolData(query.substitution(executedQuery))
      result
  */
  /*

    def joinSymbolData(queryNonRecursive: Optimized): Set[Substitution] =
      val rows = queryNonRecursive.dataMap
      val relations = queryNonRecursive.predicates
      val attributes = queryNonRecursive.variables
      val results = joinSymbolData(rows, relations, attributes)
      results
  */


  /*

    def joinDataRecursive(nonRecursiveQuery: Optimized, headOnlyQuery: Optimized, substitution: Substitution, crrDepth: Int = 0): Set[Substitution] = {
      val executedSubstitution = execute(nonRecursiveQuery, substitution)
      val foundSubstitutions = joinSymbolData(nonRecursiveQuery.substitution(executedSubstitution))
      if foundSubstitutions.nonEmpty && crrDepth < recursiveDepth then
        val crrRecursiveResults = foundSubstitutions.flatMap(crrSubstitution => {
          joinSymbolData(headOnlyQuery.substitution(crrSubstitution))
        })
        if crrRecursiveResults.isEmpty then {
          foundSubstitutions.flatMap(crrSubstitution => {
            joinDataRecursive(nonRecursiveQuery, headOnlyQuery, crrSubstitution, crrDepth + 1)
          })
        }
        else
          crrRecursiveResults.map(crrSubstitution => crrSubstitution.composition(substitution))
      else
        Set()
    }
  */

  /*

    def joinParallelRecursive(nonRecursiveQuery: Optimized, headOnlyQuery: Optimized, substitution: Substitution, crrDepth: Int = 0): Set[Substitution] = {
      val executedSubstitution = execute(nonRecursiveQuery, substitution)
      val foundSubstitutions = joinParallel(nonRecursiveQuery.substitution(executedSubstitution))
      if foundSubstitutions.nonEmpty && crrDepth < recursiveDepth then
        val crrRecursiveResults = foundSubstitutions.flatMap(crrSubstitution => {
          joinSymbolData(headOnlyQuery.substitution(crrSubstitution))
        })
        if crrRecursiveResults.isEmpty then {
          foundSubstitutions.flatMap(crrSubstitution => {
            joinParallelRecursive(nonRecursiveQuery, headOnlyQuery, crrSubstitution, crrDepth + 1)
          })
        }
        else
          crrRecursiveResults.map(crrSubstitution => crrSubstitution.composition(substitution))
      else
        Set()
    }
  */
  /*

    def joinBitmapRecursive(nonRecursiveQuery: Optimized, headOnlyQuery: Optimized, substitution: Substitution, crrDepth: Int = 0): Set[Substitution] = {
      val executedSubstitution = execute(nonRecursiveQuery, substitution)
      val foundSubstitutions = joinBitmap(nonRecursiveQuery.substitution(executedSubstitution))
      if foundSubstitutions.nonEmpty && crrDepth < recursiveDepth then
        val crrRecursiveResults = foundSubstitutions.flatMap(crrSubstitution => {
          joinSymbolData(headOnlyQuery.substitution(crrSubstitution))
        })
        if crrRecursiveResults.isEmpty then {
          foundSubstitutions.flatMap(crrSubstitution => {
            joinBitmapRecursive(nonRecursiveQuery, headOnlyQuery, crrSubstitution, crrDepth + 1)
          })
        }
        else
          crrRecursiveResults.map(crrSubstitution => crrSubstitution.composition(substitution))
      else
        Set()
    }
  */
  /*

    def joinRoaringRecursive(nonRecursiveQuery: Optimized, headOnlyQuery: Optimized, substitution: Substitution, crrDepth: Int = 0): Set[Substitution] = {
      val executedSubstitution = execute(nonRecursiveQuery, substitution)
      val foundSubstitutions = joinRoaring(nonRecursiveQuery.substitution(executedSubstitution))
      if foundSubstitutions.nonEmpty && crrDepth < recursiveDepth then
        val crrRecursiveResults = foundSubstitutions.flatMap(crrSubstitution => {
          joinSymbolData(headOnlyQuery.substitution(crrSubstitution))
        })
        if crrRecursiveResults.isEmpty then {
          foundSubstitutions.flatMap(crrSubstitution => {
            joinRoaringRecursive(nonRecursiveQuery, headOnlyQuery, crrSubstitution, crrDepth + 1)
          })
        }
        else
          crrRecursiveResults.map(crrSubstitution => crrSubstitution.composition(substitution))
      else
        Set()
    }

  */
  /*

    def joinDataRecursive(originalQuery: Optimized): Set[Substitution] =
      val q_head = originalQuery.getRecursive()
      val q_non = originalQuery.getNonRecursive()
      joinDataRecursive(q_non, q_head, Substitution())
  */
  /*
    def joinBitmap(query: Optimized): Set[Substitution] =
      val rows = query.rowsBitmap
      val relations = query.predicates
      val attributes = query.variables
      joinBitmap(rows, relations, attributes)*/
  /*
    def joinBitmapRecursive(originalQuery: Optimized): Set[Substitution] =
      val q_head = originalQuery.getRecursive()
      val q_non = originalQuery.getNonRecursive()
      joinBitmapRecursive(q_non, q_head, Substitution())*/
  /*
    def joinRoaring(query: Optimized): Set[Substitution] =
      val rows = query.roaringBitmap
      val relations = query.predicates
      val attributes = query.variables
      joinRoaring(rows, relations, attributes)*/
  /*
    def joinRoaringRecursive(originalQuery: Optimized): Set[Substitution] =
      val q_head = originalQuery.getRecursive()
      val q_non = originalQuery.getNonRecursive()
      joinRoaringRecursive(q_non, q_head, Substitution())*/
  /*
    def joinParallel(query: Optimized): Set[Substitution] =
      val rows = query.rows
      val relations = query.predicates
      val attributes = query.variables
      joinParallel(rows, relations, attributes)*/
  /*
    def joinParallelRecursive(originalQuery: Optimized): Set[Substitution] =
      val q_head = originalQuery.getRecursive()
      val q_non = originalQuery.getNonRecursive()
      joinParallelRecursive(q_non, q_head, Substitution())*/


}