package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.Variable

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

class EngineRoaringParallel(db:Database, depth:Int) extends EngineRoaringSerial(db, depth) {

  /*override def join(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val contextMap = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      val headPredicate = context.getHead
      val contextId = context.getContextId(substitution)
      if context.isTarget then context.setSubstitution(substitution)

      if programCache.contains(contextId) then
        //println("Hit...")
        val crrSubstitutions = programCache.get(contextId)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateUnion(headPredicate, crrPredicates.toArray))
      else if !context.isFunctional || context.isTarget then {
        //println("Miss...")
        val crrSubstitutions = join(contextMap, context, context)
          .map(substitution => substitution.get(headPredicate.getVariables)) //++ atomSubstitutions(headPredicate, substitution)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateUnion(headPredicate, crrPredicates.toArray))
        programCache.update(contextId, crrSubstitutions)
      }

    })
    substitutions

  }*/


  override def join(contextMap: Map[Int, Array[ExecutionContext]], programContext: ExecutionContext, currentContext: ExecutionContext): Set[Substitution] = {

    if (currentContext.getDepth > recursiveDepth || currentContext.emptyAttributes) then
      Set(Substitution())
    else {
      val newSubstitution = currentContext.getSubstitution
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable

      val activeDomain = active(contextMap, programContext, nextContext)
      val count = activeDomain.size
      val results = activeDomain.par.flatMap(value => {

        val rowMap = filterRoaring(currentContext.getRowMap, currentContext.getRelations, nextVariable, value)
        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setRowMap(rowMap)

        val partialResults = join(contextMap, programContext, newContext)

        val substitutions = partialResults.map(partial => {
          partial.add(nextVariable, value.copy(nextVariable.getName))
        })

        substitutions

      }).toArray.toSet

      results
    }
  }
}
