package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

class EngineRoaringSerial(db:Database, depth:Int) extends Engine(db, depth) {


  override def execute(contextMap: Map[Int, Array[ExecutionContext]],
                           programContext: ExecutionContext,
                           context: ExecutionContext, predicate: Predicate, predicateIndex: Int, attribute: Variable): Set[Variable] = {
    if attribute.isSymbol && predicate.isFunctional && predicate.containsInput(attribute) then {
      //No need execution or context switch
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
      val rowMap = context.getRowMap
      val values = dataMap.getOrElse(pid, Array[Predicate]())
      val bitset = rowMap.getOrElse(pid, RoaringBitmap()).toArray
      val targetName = attribute.getName
      val crrResults = bitset.map(values).map(predicate => predicate.getVariable(position).copy(attribute.getName))
        .toSet

      if !rowMap.contains(pid) && crrResults.isEmpty then {
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

  }

  def join(programs: Array[Optimized], substitution: Substitution = Substitution()): Set[Substitution] = {

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
          .foreach(other => other.updateRowData(headPredicate, crrPredicates.toArray))
      }
    })

    substitutions
  }

  override def join(contextMap: Map[Int, Array[ExecutionContext]], programContext: ExecutionContext, currentContext: ExecutionContext): Set[Substitution] = {

    if (currentContext.getDepth > recursiveDepth || currentContext.emptyAttributes) then
      Set(Substitution())
    else {
      val newSubstitution = currentContext.getSubstitution
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable

      val activeDomain = active(contextMap, programContext, nextContext)
      val count = activeDomain.size
      val results = activeDomain.flatMap(value => {

        val filteredRowMap = filterRoaring(currentContext.getRowMap, currentContext.getRelations, nextVariable, value)
        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setRowMap(filteredRowMap)

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
