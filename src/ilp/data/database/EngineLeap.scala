package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.Variable

class EngineLeap(db: Database, recursiveDepth: Int) extends EngineSerial(db, recursiveDepth) {


  override def join(programs: Array[Optimized], substitution: Substitution): Set[Substitution] = {
    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val program = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      if context.isTarget then context.setSubstitution(substitution)
      if !context.isFunctional || context.isTarget then {
        val headPredicate = context.getHead
        val crrSubstitutions = simpleJoin(program, context)
          .map(substitution => substitution.get(headPredicate.getVariables)) //++ atomSubstitutions(headPredicate, substitution)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateUnion(headPredicate, crrPredicates.toArray))
      }
    })

    substitutions
  }


  override def join(programs: Array[Optimized], callPredicate: Predicate): Set[Substitution] = {

    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val program = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      if context.isTarget then {
        context.setSubstitution(callPredicate.toSubstitution(context.getHead))
      }
      if !context.isFunctional || context.isTarget then {
        val headPredicate = context.getHead
        val crrSubstitutions = simpleJoin(program, context)
          .map(substitution => substitution.get(headPredicate.getVariables)) //++ atomSubstitutions(headPredicate, substitution)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateUnion(headPredicate, crrPredicates.toArray))
      }
    })

    substitutions
  }


  def simpleSwitch(program:Map[Int, Array[ExecutionContext]], context: ExecutionContext, predicate: Predicate,
                   attribute: Variable, predicateId: Int, position: Int): Set[Variable] = {
    val executionId = executionCache.id(context, predicate, position)
    val existingSubstitutions = if !executionCache.contains(executionId) then {
      val results = program(predicateId)
        .flatMap(currentContext => {
          currentContext.switchContext(context.getSubstitution, predicate, position, context.getDepth + 1)
        }).map(currentContext => {
          val substitutions = simpleJoin(program, currentContext)
          (currentContext, substitutions)
        }).toSet

      executionCache.update(executionId, results)
      results
    }
    else {
      executionCache.getFlat(executionId)
    }

    val targetName = attribute.getName
    val variables = existingSubstitutions.flatMap { case (currentContext, substitutions) => {
      val newVariable = currentContext.getHead.getVariable(position)
      substitutions.flatMap(substitution => substitution.valueByVariable(newVariable, targetName)
        .map(variable => variable))
    }
    }

    variables
  }

  def simpleSwitchSubstitutions(contextMap:Map[Int, Array[ExecutionContext]], context: ExecutionContext, predicate: Predicate, position: Int): Set[Substitution] = {
    val executionId = executionCache.id(context, predicate, position)
    val existingSubstitutions = if !executionCache.contains(executionId) then {
      val identifier = predicate.identifier()
      /*
      if !contextMap.contains(identifier) then
        val debuf = 0*/

      val results = contextMap.getOrElse(identifier, Array[ExecutionContext]())
        .flatMap(currentContext => {
          currentContext.switchContext(context.getSubstitution, predicate, position, context.getDepth + 1)
        }).map(currentContext => {
          val substitutions = simpleJoin(contextMap, currentContext)
          (currentContext, substitutions)
        }).toSet

      executionCache.update(executionId, results)
      results
    }
    else {



      val results = executionCache.getFlat(executionId)
      results
    }

    val main = context.getSubstitution
    val substitutionList = existingSubstitutions.flatMap { case (currentContext, substitutions) => {
      substitutions.map(substitution => {
        val switchBackSubstitution = currentContext.getBack(substitution, predicate)
        main.composition(switchBackSubstitution)
      })
    }
    }

    substitutionList
  }

  def simpleRetrieve(contextMap:Map[Int, Array[ExecutionContext]], context: ExecutionContext, predicate: Predicate, attribute: Variable, index: Int): Set[Variable] =
    //Retrieve the result
    val predicateId = predicate.identifier()
    val pid = predicate.identifier(index)
    val position = predicate.getPosition(attribute)
    val dataMap = context.getDataMap
    val targetName = attribute.getName
    val existingResults = dataMap.getOrElse(pid, Array[Predicate]())
      .map(predicate => predicate.getVariable(position).copy(targetName))
      .filter(variable => attribute.equalValue(variable)).toSet
    if existingResults.isEmpty && contextMap.contains(predicateId) then
      simpleSwitch(contextMap, context, predicate, attribute, predicateId, position)
    else
      existingResults


  def simpleActive(contextMap:Map[Int, Array[ExecutionContext]], context: ExecutionContext): Set[Variable] = {
    val attribute = context.getTargetVariable
    val substitution = context.getSubstitution
    val domains = context.getRelations.zipWithIndex.flatMap {
      case (predicate, index) => {
        if attribute.isSymbol && predicate.isFunctional && !predicate.isRecursive && predicate.contains(attribute) && context.executable(predicate) then {
          Some(Set(attribute))
        }
        else if predicate.contains(attribute) then
          Some(simpleRetrieve(contextMap, context, predicate, attribute, index))
        else
          None
      }
    }

    intersection(domains)
  }


  def simpleExecute(context: ExecutionContext): Option[ExecutionContext] = {
    val rule = context.getRule
    var main = context.getSubstitution

    var executed = false
    rule.getQuery.getBody
      .filter(predicate => !predicate.isRecursive && predicate.isFunctional && context.canExecute(predicate))
      .foreach(predicate => {
        val newPredicate = predicate.substitution(main)
          .asPredicate()

        if newPredicate.isDefinite && !newPredicate.isExecutable then return None
        else if newPredicate.isExecutable then
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


      })
    Some(context.newContext(main))
  }

  def simpleRetrieveSubstitutions(contextMap:Map[Int, Array[ExecutionContext]], context: ExecutionContext, predicate: Predicate, index: Int): Set[Substitution] =
    //Retrieve the result
    val predicateId = predicate.identifier()
    val pid = predicate.identifier(index)
    val dataMap = context.getDataMap
    val mainSubstitution = context.getSubstitution

    val existingResults = dataMap.getOrElse(pid, Array[Predicate]())
      .map(subPredicate => subPredicate.toSubstitution(predicate))
      .toSet

    if (predicate.isFunctional || predicate.isRecursive) && existingResults.isEmpty then
      simpleSwitchSubstitutions(contextMap, context, predicate, index)
        .filter(substitution => !mainSubstitution.hasConflict(substitution))
    else
      existingResults.map(substition => context.getSubstitution.composition(substition))
        .filter(substitution => !mainSubstitution.hasConflict(substitution))

  def simpleExecute(contextMap:Map[Int, Array[ExecutionContext]], executionContext: ExecutionContext, substitutions: Set[Substitution], predicate: Predicate, index: Int): Set[Substitution] =
    var results = Set[Substitution]()
    substitutions.foreach(main => {
      val newPredicate = predicate.substitution(main)
        .asPredicate()
      if newPredicate.isDefinite/* && newPredicate.isExecutable*/ then
        newPredicate.execute().foreach(substitution => {
          val composed = main.composition(substitution)
          if !main.hasConflict(composed) then
            results += composed
        })
      else if executionContext.getDepth <= recursiveDepth then
        val newContext = executionContext.newContext(main).incrementDepth()
        simpleRetrieveSubstitutions(contextMap, newContext, newPredicate, index).foreach(substitution => {
          val composed = main.composition(substitution)
          if !main.hasConflict(composed) then
            results += composed
        })
    })

    results

  def simpleExecuteSubstitutions(contextMap:Map[Int, Array[ExecutionContext]], context: ExecutionContext, substitutions: Set[Substitution]): Set[Substitution] = {
    val rule = context.getRule
    var newSubstitutions = substitutions
    rule.getQuery.getBody
      .zipWithIndex
      .filter { case (predicate, index) => {
        !predicate.isRecursive /*&& /*predicate.isFunctional && */context.executable(predicate)*/
      }}
      .foreach { case (predicate, index) => {
        newSubstitutions = simpleExecute(contextMap, context, newSubstitutions, predicate, index)
      }
      }

    newSubstitutions

  }


  //Find all input values for all items
  def simpleJoin(program:Map[Int, Array[ExecutionContext]], executionContext: ExecutionContext): Set[Substitution] = {


    val substitutions = simpleExecuteSubstitutions(program, executionContext, Set(executionContext.getSubstitution))
    substitutions.flatMap(substitution => join(program, executionContext.newContext(substitution)))

  }

  def join(contextMap:Map[Int, Array[ExecutionContext]], currentContext: ExecutionContext): Set[Substitution] = {
    if (currentContext.getDepth > recursiveDepth || currentContext.emptyAttributes) then
      Set[Substitution](Substitution())
    else {
      val newSubstitution = currentContext.getSubstitution
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable

      val activeDomain = simpleActive(contextMap, nextContext)
      val count = activeDomain.size
      val results = activeDomain.flatMap(value => {

        val filteredMap = filterData(currentContext.getDataMap, currentContext.getRelations, nextVariable, value)
        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setDataMap(filteredMap)

        val partialResults = join(contextMap, newContext)

        val substitutions = partialResults.map(partial => {
          //partial.replaceNew(nextVariable, value.copy(nextVariable.getName))
          partial.add(nextVariable, value.copy(nextVariable.getName))
        })

        substitutions

      }).toArray.toSet

      results
    }
  }

}
