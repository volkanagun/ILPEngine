package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Substitution}
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

import java.text.AttributedCharacterIterator.Attribute
import java.util.concurrent.locks.ReentrantLock
import scala.collection.concurrent.TrieMap as ConcurrentMap
import scala.collection.immutable.{BitSet, Set}
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable


class EngineSerial(val db: Database, recursiveDepth: Int = 10) extends Engine(db, recursiveDepth) {

  /*
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
            val substitutions = joinSerial(contextMap, programContext, currentContext)
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
    }*/


  /*
    */


  /*

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
  }*/


  /*
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
            .foreach(other => other.updateData(headPredicate, crrPredicates.toArray))
        }

      })

      substitutions
    }*/


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
          .foreach(other => other.updateUnion(headPredicate, crrPredicates.toArray))
      }
    })

    substitutions
  }

  def join(programs: Array[Optimized], callPredicate: Predicate): Set[Substitution] = {

    val contextProgram = programs.map(rule => ExecutionContext(rule, Substitution()))
    val contextMap = contextProgram
      .groupBy { context => context.getHead.identifier() }

    var substitutions = Set[Substitution]()
    contextProgram.foreach(context => {
      if context.isTarget then {
        context.setSubstitution(callPredicate.toSubstitution(context.getHead))
      }
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
      Set[Substitution](Substitution())
    else {
      val newSubstitution = currentContext.getSubstitution
      val nextContext = currentContext.nextContext(newSubstitution)
      val nextVariable = nextContext.getTargetVariable

      val activeDomain = active(contextMap, programContext, nextContext)
      val count = activeDomain.size
      val results = activeDomain.flatMap(value => {

        val filteredMap = filterData(currentContext.getDataMap, currentContext.getRelations, nextVariable, value)
        val newContext = nextContext.newContext(newSubstitution.composition(value))
          .setDataMap(filteredMap)

        val partialResults = join(contextMap, programContext, newContext)

        val substitutions = partialResults.map(partial => {
          //partial.replaceNew(nextVariable, value.copy(nextVariable.getName))
          partial.add(nextVariable, value.copy(nextVariable.getName))
        })

        substitutions

      }).toArray.toSet

      results
    }
  }




  /*
    override def execute(contextMap: Map[Int, Array[ExecutionContext]],
                      programContext: ExecutionContext,
                      context: ExecutionContext, predicate: Predicate, predicateIndex:Int, attribute: Variable): Set[Variable] = {
      val result = if attribute.isSymbol && predicate.isFunctional && predicate.containsInput(attribute) then {
        //No need execution or context switch
        Set[Variable](attribute)
      }
      else if !predicate.isRecursive && predicate.isFunctional && context.canExecute(predicate) then
        val result = executeActive(context)
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
      else{
        Set[Variable]()
      }

      result
    }

    */
  /*


  */


  /* def executeRoaringParallel(contextMap: Map[Int, Array[ExecutionContext]],
                            programContext: ExecutionContext,
                            context: ExecutionContext, predicate: Predicate, predicateIndex:Int, attribute: Variable): Set[Variable] = {
     val result = if attribute.isSymbol && predicate.isFunctional && predicate.containsInput(attribute) then {
       //No need execution or context switch
       Set[Variable](attribute)
     }
     else if !predicate.isRecursive && predicate.isFunctional && context.canExecute(predicate) then
       val result = executeActive(context)
       if result.isDefined && result.get.isEmpty then {
         //No confliction exists but switch maybe needed
         val identifier = predicate.identifier()
         val position = predicate.getPosition(attribute)
         val switchResult = roaringParallelSwitch(contextMap, programContext, context, predicate, attribute, identifier, position)
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
         val switchResult = roaringParallelSwitch(contextMap, programContext, context, predicate, attribute, predicateId, position)
         switchResult
       }
       else if context.isRecursive && crrResults.isEmpty then
         val switchResult = roaringParallelSwitch(contextMap, programContext, context, predicate, attribute, predicateId, position)

         switchResult
       else
         crrResults
     }
     else{
       Set[Variable]()
     }
     result
   }*/


}