package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.Variable

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

class EngineParallel(db:Database, depth:Int) extends Engine(db, depth) {

  override def join(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val contextMap = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      if context.isTarget then context.setSubstitution(substitution)
      if !context.isFunctional || context.isTarget then {
        val headPredicate = context.getHead
        val crrSubstitutions = join(contextMap, context, context)
          .map(substitution => substitution.get(headPredicate.getVariables)) //++ atomSubstitutions(headPredicate, substitution)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateUnion(headPredicate, crrPredicates.toArray))
      }

    })

    substitutions
  }

  def join(contextMap: Map[Int, Array[ExecutionContext]], programContext: ExecutionContext, currentContext: ExecutionContext): Set[Substitution] = {

    if (currentContext.getDepth > recursiveDepth || currentContext.emptyAttributes) then
      Set(Substitution())
    else {
      val newSubstitution = currentContext.getSubstitution
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable

      val activeDomain = active(contextMap, programContext, nextContext)
      val count = activeDomain.size
      val results = activeDomain.par.flatMap(value => {

        val filteredMap = filterData(currentContext.getDataMap, currentContext.getRelations, nextVariable, value)
        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setDataMap(filteredMap)

        val partialResults = join(contextMap, programContext, newContext)

        val substitutions = partialResults.map(partial => {
          partial.add(nextVariable, value.copy(nextVariable.getName))
        })

        substitutions

      }).toArray.toSet

      results
    }
  }

  /*def switch(contextMap: Map[Int, Array[ExecutionContext]],
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

  def active(contextMap: Map[Int, Array[ExecutionContext]],
                     programContext: ExecutionContext,
                     nextContext: ExecutionContext): Set[Variable] = {

    val dataMap = nextContext.getDataMap
    val attribute = nextContext.getTargetVariable
    val substitution = nextContext.getSubstitution
    val domains = nextContext.getRelations.zipWithIndex.flatMap {
      case (predicate, index) => {
        if predicate.contains(attribute) && predicate.isFunctional && !nextContext.canExecute(predicate) then
          None
        else if predicate.contains(attribute) then
          Some(execute(contextMap, programContext, nextContext, predicate, index, attribute))
        else
          None
      }
    }

    intersection(domains)
  }*/

}
