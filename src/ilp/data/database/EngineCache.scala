package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.program.Substitution

class EngineCache(db:Database, depth:Int) extends EngineParallel(db, depth) {

  override def join(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val contextMap = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      val headPredicate = context.getHead
      val contextId = context.getContextId(substitution)
      if context.isTarget then context.setSubstitution(substitution)

      if programCache.contains(contextId) then
        val crrSubstitutions = programCache.get(contextId)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateData(headPredicate, crrPredicates.toArray))
      else if !context.isFunctional || context.isTarget then {
        val crrSubstitutions = join(contextMap, context, context)
          .map(substitution => substitution.get(headPredicate.getVariables)) //++ atomSubstitutions(headPredicate, substitution)
        val crrPredicates = context.get(crrSubstitutions)
        substitutions = substitutions ++ (if context.isTarget then crrSubstitutions else Set())
        contextProgram.filter(other => context.calledFrom(other))
          .foreach(other => other.updateData(headPredicate, crrPredicates.toArray))
        programCache.update(contextId, crrSubstitutions)
      }

    })

    substitutions
  }
}
