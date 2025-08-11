package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Substitution}
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import java.util.concurrent.locks.ReentrantLock
import scala.collection.immutable.Set
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable


class EngineOLD(val database: Database, val recursiveDepth: Int = 10) extends Serializable {

  val executionCache = ExecutionCache()
  val programCache = ProgramCache()
  val rentrantLock = new ReentrantLock()

  def getDatabase() = database

  def validHypothesis(hypothesis: Hypothesis): Boolean =
    database.getBias.getHypothesis(hypothesis).isDefined

  def atomSubstitutions(headPredicate: Predicate, substitution: Substitution): Set[Substitution] = {
    val substitutions = database.getSubstitutions(headPredicate)
    substitutions.filter(crrSubstitution => !crrSubstitution.conflicts(substitution))
  }


  def intersection(domains: Array[Set[Variable]]): Set[Variable] =
    if domains.isEmpty then {
      Set.empty
    }
    else {
      domains.reduce(_ intersect _)
    }

  def filterData(dataMap: Map[Int, Array[Predicate]], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, Array[Predicate]] =
    val newMap = relations.zipWithIndex
      .flatMap { case (predicate, index) => {
        val pid = predicate.identifier(index)
        if dataMap.contains(pid) then
          val crrData = dataMap(pid)
          if predicate.contains(attribute) then
            val indice = predicate.getPosition(attribute)
            val newPredicates = crrData.filter(predicate => predicate.getVariable(indice).equalValue(value))
            Some(pid -> newPredicates)
          else
            Some(pid -> crrData)
        else
          None
      }
      }.toMap

    newMap

  def filterRoaring(rowMap: Map[Int, RoaringBitmap], relations: Array[Predicate], attribute: Variable, value: Variable): Map[Int, RoaringBitmap] =
    val valueHash = value.hashCode()
    val newMap = relations.filter(predicate=> !predicate.isFunctional && !predicate.isRecursive).zipWithIndex
      .flatMap { case (predicate, position) => {
        val pid = predicate.identifier(position)
        if rowMap.contains(pid) then
          val crrRows = rowMap(pid)
          if predicate.contains(attribute) then
            val predicateId = predicate.identifier()
            val indexMap = database.getIndex
            val crrIndex = indexMap(predicateId)
            val indice = predicate.getPosition(attribute)
            val newRows = crrIndex.getHavingRows(crrRows, valueHash, indice)
            Some(pid -> newRows)
          else
            Some(pid -> crrRows)
        else
          None
      }
      }.toMap

    newMap



  def roaringSerialSwitch(contextMap: Map[Int, Array[ExecutionContext]],
                     programContext: ExecutionContext,
                     nextContext: ExecutionContext, predicate: Predicate,
                     attribute: Variable, predicateId: Int, position: Int): Set[Variable] = {

    val executionId = executionCache.id(nextContext, predicate)
    val existingSubstitutions = executionCache.get(executionId)

    val items = (if contextMap.contains(predicateId) then {
      val results = contextMap(predicateId)
        .flatMap(currentContext => {
          currentContext.switchContext(nextContext.getSubstitution, predicate, position, nextContext.getDepth + 1)
        }).map(currentContext => {
          val substitutions = joinRoaringSerial(contextMap, programContext, currentContext)
          (currentContext, substitutions)
        }).toSet

      executionCache.update(executionId, results)
      results
    }
    else {
      Set[(ExecutionContext, Set[Substitution])]()
    })


    //Switch back to current context
    val targetName = attribute.getName
    val variables = items.flatMap { case (currentContext, substitutions) => {
      val newVariable = currentContext.getHead.getVariable(position)
      substitutions.flatMap(substitution => substitution.valueByVariable(newVariable, targetName)
        .map(variable => variable))
    }
    }

    variables
  }



  def activeRoaringSerial(contextMap: Map[Int, Array[ExecutionContext]],
                     programContext: ExecutionContext,
                     nextContext: ExecutionContext): Set[Variable] = {


    val attribute = nextContext.getTargetVariable
    val substitution = nextContext.getSubstitution
    val domains = nextContext.getRelations.zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.contains(attribute) && predicate.isFunctional && !nextContext.canExecute(predicate) then
          None
        else if predicate.contains(attribute) then
          Some(executeRoaringSerial(contextMap, programContext, nextContext, predicate, index, attribute))
        else
          None
      }
    }

    intersection(domains)
  }


  def joinRoaringSerial(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val contextMap = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      if context.isTarget then context.setSubstitution(substitution)
      if !context.isFunctional || context.isTarget then {
        val headPredicate = context.getHead
        val crrSubstitutions = joinRoaringSerial(contextMap, context, context)
          .map(substitution => substitution.get(headPredicate.getVariables)) ++ atomSubstitutions(headPredicate, substitution)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateRowData(headPredicate, crrPredicates.toArray))
      }
    })

    substitutions
  }

  def joinRoaringSerial(contextMap: Map[Int, Array[ExecutionContext]], programContext: ExecutionContext, currentContext: ExecutionContext): Set[Substitution] = {

    if (currentContext.getDepth > recursiveDepth || currentContext.emptyAttributes) then
      Set(Substitution())
    else {
      val newSubstitution = currentContext.getSubstitution
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable

      val activeDomain = activeRoaringSerial(contextMap, programContext, nextContext)
      val count = activeDomain.size
      val results = activeDomain.flatMap(value => {

        val filteredRowMap = filterRoaring(currentContext.getRowMap, currentContext.getRelations, nextVariable, value)
        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setRowMap(filteredRowMap)

        val partialResults = joinRoaringSerial(contextMap, programContext, newContext)

        val substitutions = partialResults.map(partial => {
          partial.replaceNew(nextVariable, value.copy(nextVariable.getName))
        })

        substitutions

      }).toArray.toSet

      results
    }
  }



  def executeRoaringSerial(contextMap: Map[Int, Array[ExecutionContext]],
                           programContext: ExecutionContext,
                           context: ExecutionContext, predicate: Predicate, predicateIndex:Int, attribute: Variable): Set[Variable] = {
    if attribute.isSymbol && predicate.isFunctional && predicate.containsInput(attribute) then {
      //No need execution or context switch
      Set[Variable](attribute)
    }
    else if !predicate.isRecursive && predicate.isFunctional && context.canExecute(predicate) then
      val result = executeActive(context)
      if result.isDefined && result.get.isEmpty then {
        //No confliction exists but switch maybe needed
        val identifier = predicate.identifier()
        val position = predicate.getPosition(attribute)
        val switchResult = roaringSerialSwitch(contextMap, programContext, context, predicate, attribute, identifier, position)
        checkConfliction(context, switchResult)
      }
      else if result.isDefined && result.get.nonEmpty then {
        //Execution is successfully return value
        result.get
      }
      else {
        //Has confliction
        Set[Variable]()
      }
    else if predicate.contains(attribute) then {
      //No need execution lookup results from cache or switch context
      val predicateId = predicate.identifier()
      val pid = predicate.identifier(predicateIndex)
      val position = predicate.getPosition(attribute)
      val dataMap = context.getDataMap
      val rowMap = context.getRowMap
      val values = dataMap.getOrElse(pid, Array[Predicate]())
      val bitset = rowMap.getOrElse(pid, RoaringBitmap()).toArray
      val targetName = attribute.getName
      val crrResults = bitset.map(values).map(predicate=> predicate.getVariable(position).copy(attribute.getName))
        .toSet

      if !rowMap.contains(pid) && crrResults.isEmpty then {
        val switchResult = roaringSerialSwitch(contextMap, programContext, context, predicate, attribute, predicateId, position)
        switchResult
      }
      else if context.isRecursive && crrResults.isEmpty then
        val switchResult = roaringSerialSwitch(contextMap, programContext, context, predicate, attribute, predicateId, position)
        //checkConfliction(context, switchResult)
        switchResult
      else
        crrResults
    }
    else{
      Set[Variable]()
    }

  }


  def executeActive(context: ExecutionContext): Option[Set[Variable]] =


    val result = executeResult(context)
    if result.nonEmpty then {
      val (substitution, executed) = result.get
      if executed then
        val targetVariable = context.getTargetVariable
        val results = substitution.valueByVariable(targetVariable)
        Some(Set(results.get))
      else
        Some(Set())
    }
    else {
      None
    }

  def execute(context: ExecutionContext): Option[Substitution] = {
    val rule = context.getRule
    var main = context.getSubstitution

    rule.getQuery.getBody
      .filter(predicate => predicate.isFunctional)
      .foreach(predicate => {
        val newPredicate = predicate.substitution(main)
          .asPredicate()
        if newPredicate.isDefinite then {
          if newPredicate.isExecutable then {
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

  def checkConfliction(context:ExecutionContext, attributes:Set[Variable]):Set[Variable]=
    val substitution = context.getSubstitution
    val filters = attributes.map(attribute=> (substitution.valueByVariable(attribute), attribute))
      .filter{case(value, attribute) => value.nonEmpty}

    if filters.isEmpty then
      //No confliction
      attributes
    else {
      //May have confliction
      filters.filter {case(value, attribute)=> value.get.equalType(attribute) && value.get.equalValue(attribute)}
        .map{case(value, attribute)=> attribute}
    }


  def executeResult(context: ExecutionContext): Option[(Substitution, Boolean)] = {
    val rule = context.getRule
    var main = context.getSubstitution
    val targetVariable = context.getTargetVariable
    var executed = false
    rule.getQuery.getBody
      .filter(predicate => predicate.isFunctional && predicate.contains(targetVariable) && !predicate.containsInput(targetVariable))
      .foreach(predicate => {
        val newPredicate = predicate.substitution(main)
          .asPredicate()
        if newPredicate.isDefinite then {
          if newPredicate.isExecutable then {
            newPredicate.execute().foreach(newSubstitution => {
              val newComposed = main.composition(newSubstitution)
              if main.hasConflict(newComposed) then {
                return None
              }
              else {
                executed = true
                main = newComposed
              }
            })
          }
          else {
            return None
          }
        }
      })

    Some((main, executed))
  }


}