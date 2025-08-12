package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.{Query, Substitution}
import ilp.data.variables.{Num, Variable}
import org.roaringbitmap.RoaringBitmap

final class ExecutionContext(private var rule: Optimized,
                             private var dataMap: Map[Int, Array[Predicate]],
                             private var originalMap: Map[Int, Array[Predicate]],
                             private var rowMap: Map[Int, RoaringBitmap],
                             private var originalRowMap: Map[Int, RoaringBitmap],
                             private var substitution: Substitution,
                             private val targetVariable: Variable,
                             private var relations: Array[Predicate],
                             private var attributes: Array[Variable],
                             private var depth: Int = 0) extends Serializable{


  def copy():ExecutionContext =
    val nrule = rule
    val ndataMap = dataMap
    val noriginalMap = originalMap
    val nrowMap = rowMap
    val noriginalRowMap = originalRowMap
    val nsubstitution = substitution.copy()
    val ntarget = targetVariable.copy()
    val nrelations = relations
    val nattributes = attributes
    val ndepth = depth
    new ExecutionContext(nrule, ndataMap, noriginalMap, nrowMap, noriginalRowMap, nsubstitution, ntarget, nrelations, nattributes, ndepth)




  def get(substitutions: Set[Substitution]): Set[Predicate] = {
    val headVariables = getHead.getVariables
    substitutions
      .map(substitution => rule.getHead.substitution(substitution).asPredicate())
  }

  def getExecutionId(predicate: Predicate): Int =
    (predicate.getInput
      .filter(variable => substitution.contains(variable))
      .flatMap(variable => substitution.valueByVariable(variable))
      .map(symbol => symbol.hashCode()) :+ predicate.hashCode())
      .foldRight[Int](1) { case (code, main) => main * 7 + code }

  def canSwitchContext(substitution: Substitution): Boolean = {
    val hasInputVariables = getQuery.getInputVariables.forall(variable => substitution.hasVariable(variable))
    hasInputVariables
  }

  def canExecute(predicate: Predicate):Boolean =
    substitution.hasInputs(predicate)

  def conflictContext(newContext: ExecutionContext): Boolean = {
    val result = depth == newContext.getDepth && substitution.conflicts(newContext.getSubstitution)
    if result then {
      true
    }
    else false
  }

  def switchContext(newSubstitution: Substitution, calledFrom: Predicate, position: Int, newDepth: Int): Option[ExecutionContext] = {
    val currentHead = getHead
    val switchedSubstitution = calledFrom.callSubstitution(currentHead, newSubstitution)
    if canSwitchContext(switchedSubstitution) then
      Some(new ExecutionContext(rule, originalMap, originalMap, originalRowMap, originalRowMap, switchedSubstitution, targetVariable, relations, attributes, newDepth))
    else
      None
  }

  def nextContext(newSubstitution: Substitution): ExecutionContext =
    val restAttributes = newSubstitution.compose(attributes.tail)
    val nextAttribute = newSubstitution.compose(attributes.head)
    new ExecutionContext(rule, dataMap, originalMap, rowMap, originalRowMap, newSubstitution, nextAttribute, relations, restAttributes, depth)


/*  def newContext(targetAttribute: Variable, restAttributes: Array[Variable]): ExecutionContext =
    new ExecutionContext(rule, dataMap, originalMap, rowMap, originalRowMap, substitution, targetAttribute, relations, restAttributes, depth)*/

  def newContext(substitution: Substitution): ExecutionContext =
    new ExecutionContext(rule, dataMap, originalMap, rowMap, originalRowMap, substitution, targetVariable, relations, attributes, depth)

  def calledFrom(other: ExecutionContext): Boolean =
    getQuery.calledFrom(other.getQuery)


  inline def getOriginalMap: Map[Int, Array[Predicate]] = {
    this.originalMap
  }

  def updateRowData(predicate: Predicate, array: Array[Predicate]): ExecutionContext =
    val identifier = predicate.identifier()
    val targetMap = relations.zipWithIndex.filter { case (relation, position) => identifier == relation.identifier() }
      .map { case (predicate, position) => {
        predicate.identifier(position) -> array
      }
      }.toMap

    val roaringMap = targetMap.map { case (id, predicates) => {
      val bitmap = RoaringBitmap()
      bitmap.add(Range(0, predicates.length): _*)
      id -> bitmap
    }
    }

    dataMap = dataMap ++ targetMap
    originalMap = originalMap ++ targetMap
    rowMap = rowMap ++ roaringMap
    originalRowMap = rowMap
    this

  def updateData(predicate: Predicate, set: Array[Predicate]): ExecutionContext =
    val id = predicate.identifier()
    val targetMap = relations.zipWithIndex.filter { case (relation, position) => relation.identifier() == id }
      .map { case (predicate, index) => {
        predicate.identifier(index) -> set
      }}
      .toMap

    dataMap = dataMap ++ targetMap
    originalMap = dataMap
    this

  override def toString: String =
    "Rule: " + rule.getQuery.toString + "\n" +
      "Target: " + targetVariable.toString + "\n" +
      "Attributes: " + attributes.mkString("[", ",", "]") + "\n" +
      "Substitution: " + substitution.toString + "\n" +
      "Data size: " + dataMap.map(_._2.length).mkString("[", ",", "]")

  inline def emptyAttributes = attributes.isEmpty

  inline def getContextId(substitution: Substitution): Int =
    rule.getQueryId * 7 + substitution.id()

  inline def getQuery: Query = rule.getQuery
  inline def getHead: Predicate = rule.getQuery.getHead
  inline def getHeadVariables: Array[Variable] = rule.getHead.getVariables
  inline def getTargetVariable: Variable = targetVariable
  inline def getRule: Optimized = rule
  inline def getDataMap: Map[Int, Array[Predicate]] = dataMap
  inline def getRowMap: Map[Int, RoaringBitmap] = rowMap
  inline def getResetDataMap: Map[Int, Array[Predicate]] = originalMap
  inline def getSubstitution: Substitution = substitution
  inline def getRelations: Array[Predicate] = relations
  inline def getAttributes: Array[Variable] = attributes
  inline def getDepth: Int = depth
  inline def isTarget: Boolean = rule.getTarget
  inline def isFunctional:Boolean = rule.isFunctional
  inline def isRecursive: Boolean = rule.isRecursive

  inline def setDataMap(dataMap: Map[Int, Array[Predicate]]): ExecutionContext = {
    this.dataMap = dataMap
    this
  }

  inline def setRowMap(rowMap: Map[Int, RoaringBitmap]): ExecutionContext = {
    this.rowMap = rowMap
    this
  }

  inline def setOriginalMap(originalMap: Map[Int, Array[Predicate]]): ExecutionContext = {
    this.originalMap = originalMap
    this
  }


  inline def setAttributes(newAttributes: Array[Variable]): ExecutionContext = {
    this.attributes = newAttributes
    this
  }

  inline def setSubstitution(newSubstitution: Substitution): ExecutionContext = {
    this.substitution = newSubstitution
    this
  }



}

object ExecutionContext {

  def apply(mainRule: Optimized, substitution: Substitution): ExecutionContext =
    val dataMap = mainRule.getDataMap
    val rowMap = mainRule.getRoaringMap
    val relations = mainRule.getRelations
    val attributes = mainRule.getVariables
    new ExecutionContext(mainRule, dataMap, dataMap, rowMap, rowMap, substitution, attributes.head, relations, attributes, 0)
}
