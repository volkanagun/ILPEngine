package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.Variable

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

class EngineRoaringParallel(db:Database, depth:Int) extends EngineRoaringSerial(db, depth) {

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
