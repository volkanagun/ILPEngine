package ilp.data.database

import ilp.data.optimization.{Index, Optimized}
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Substitution}
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import scala.annotation.tailrec


abstract class Engine(val database: Database, val recursiveDepth: Int = 10) extends Serializable{

  val executionCache = ExecutionCache()
  val programCache = ProgramCache()


  def join(contextMap: Map[Int, Array[ExecutionContext]], programContext: ExecutionContext, currentContext: ExecutionContext): Set[Substitution]
  def join(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution]
  def join(programs: Array[Optimized], callPredicate:Predicate):Set[Substitution]


  def getDatabase: Database = database
  def updateIndex(predicate:Predicate, results:Set[Predicate]):this.type =
    val predicateID = predicate.identifier()
    var crrIndex = database.getIndex
    if crrIndex.contains(predicateID) then
      val existingIndex = crrIndex(predicateID)
      results.foreach(predicate=> existingIndex.addIndex(predicate))
    else {
      database.addIndex(predicate, results)
    }
    this


  def validHypothesis(hypothesis: Hypothesis): Boolean =
    database.valid(hypothesis)

  def intersection(domains: Array[Set[Variable]]): Set[Variable] =
    if domains.isEmpty then {
      Set.empty
    }
    else {
      domains.reduce(_ intersect _)
    }

  def atomSubstitutions(headPredicate: Predicate, substitution: Substitution): Set[Substitution] = {
    val substitutions = database.getSubstitutions(headPredicate)
    substitutions.filter(crrSubstitution => !crrSubstitution.conflicts(substitution))
  }

  def switch(contextMap: Map[Int, Array[ExecutionContext]],
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
          val substitutions = join(contextMap, programContext, currentContext)
          (currentContext, substitutions)
        }).toSet

      executionCache.update(executionId, results)
      results
    }
    else {
      Set[(ExecutionContext, Set[Substitution])]()
    })

    val targetName = attribute.getName
    val variables = items.flatMap { case (currentContext, substitutions) => {
      val newVariable = currentContext.getHead.getVariable(position)
      substitutions.flatMap(substitution => substitution.valueByVariable(newVariable, targetName)
        .map(variable => variable))
    }
    }

    variables
  }

  def active(contextMap: Map[Int, Array[ExecutionContext]],
                            programContext: ExecutionContext,
                            nextContext: ExecutionContext): Set[Variable] = {


    val attribute = nextContext.getTargetVariable
    val substitution = nextContext.getSubstitution
    val domains = nextContext.getRelations.zipWithIndex.flatMap {
      case (predicate, index) => {
        if !predicate.isRecursive && predicate.contains(attribute) && predicate.isFunctional && !nextContext.canExecute(predicate) then
          None
        else if predicate.contains(attribute) then
          Some(execute(contextMap, programContext, nextContext, predicate, index, attribute))
        else
          None
      }
    }

    intersection(domains)
  }


  def checkConfliction(context: ExecutionContext, attributes: Set[Variable]): Set[Variable] =
    val substitution = context.getSubstitution
    val filters = attributes.map(attribute => (substitution.valueByVariable(attribute), attribute))
      .filter { case (value, attribute) => value.nonEmpty }

    if filters.isEmpty then
      //No confliction
      attributes
    else {
      //May have confliction
      filters.filter { case (value, attribute) => value.get.equalType(attribute) && value.get.equalValue(attribute) }
        .map { case (value, attribute) => attribute }
    }

  def execute(context: ExecutionContext): Option[Set[Variable]] =

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

  def execute(contextMap: Map[Int, Array[ExecutionContext]],
                      programContext: ExecutionContext,
                      context: ExecutionContext, predicate: Predicate, predicateIndex: Int, attribute: Variable): Set[Variable] = {
    val result = if !predicate.isRecursive && attribute.isSymbol && predicate.isFunctional && predicate.containsInput(attribute) then {
      Set[Variable](attribute)
    }
    else if !predicate.isRecursive && predicate.isFunctional && context.canExecute(predicate) then
      val result = execute(context)
      if result.isDefined && result.get.isEmpty then {
        //No confliction exists but switch maybe needed
        val identifier = predicate.identifier()
        val position = predicate.getPosition(attribute)
        val switchResult = switch(contextMap, programContext, context, predicate, attribute, identifier, position)
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
      val targetName = attribute.getName

      val crrResults = dataMap.getOrElse(pid, Array[Predicate]())
        .map(predicate => predicate.getVariable(position).copy(targetName))
        .filter(variable => attribute.equalValue(variable)).toSet

      if !dataMap.contains(pid) && crrResults.isEmpty then {
        val switchResult = switch(contextMap, programContext, context, predicate, attribute, predicateId, position)
        switchResult
      }
      else if context.isRecursive && crrResults.isEmpty then
        val switchResult = switch(contextMap, programContext, context, predicate, attribute, predicateId, position)
        switchResult
      else
        crrResults
    }
    else {
      Set[Variable]()
    }

    //checkConfliction(context, result)
    result
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
    val indexMap = database.getIndex
    val valueHash = value.hashCode()
    val newMap = relations.zipWithIndex
      .flatMap { case (predicate, position) => {
        val pid = predicate.identifier(position)
        if rowMap.contains(pid) then
          val crrRows = rowMap(pid)
          val predicateId = predicate.identifier()
          if indexMap.contains(predicateId) && predicate.contains(attribute) then
            val crrIndex = indexMap(predicateId)
            val indice = predicate.getPosition(attribute)
            val newRows = crrIndex.getHavingRows(crrRows, valueHash, indice)
            Some(pid -> newRows)
          else
            Some(pid -> crrRows)
        /*else if predicate.isFunctional || predicate.isRecursive then
            Some(pid -> crrRows)
        */else
          None
      }
      }.toMap

    newMap
}
