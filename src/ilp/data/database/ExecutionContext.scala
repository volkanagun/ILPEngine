package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.{Query, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.{Num, Variable}
import org.roaringbitmap.RoaringBitmap

final class ExecutionContext(var rule: Optimized,
                       var dataMap: Map[Int, Array[Predicate]],
                       var originalMap: Map[Int, Array[Predicate]],
                       var rowMap: Map[Int, RoaringBitmap],
                       var originalRowMap: Map[Int, RoaringBitmap],
                       var substitution: Substitution,
                       val targetVariable: Variable,
                       var relations: Array[Predicate],
                       var attributes: Array[Variable],
                       var depth: Int = 0) extends Serializable{


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


  def isRecursive():Boolean =
    rule.isRecursive()

  def get(substitutions: Set[Substitution]): Set[Predicate] = {
    val headVariables = getHead().getVariables()
    substitutions
      .map(substitution => rule.getHead().substitution(substitution).asPredicate())
  }

  def getExecutionId(predicate: Predicate): Int =
    (predicate.getInput()
      .filter(variable => substitution.contains(variable))
      .flatMap(variable => substitution.valueByVariable(variable))
      .map(symbol => symbol.hashCode()) :+ predicate.hashCode())
      .foldRight[Int](1) { case (code, main) => main * 7 + code }

  def canSwitchContext(substitution: Substitution): Boolean = {
    val hasInputVariables = getQuery().getInputVariables().forall(variable => substitution.hasVariable(variable))
    hasInputVariables
  }

  def conflictContext(newContext: ExecutionContext): Boolean = {
    val result = depth == newContext.getDepth() && substitution.conflicts(newContext.getSubstitution())
    if result then {
      true
    }
    else false
  }

  def switchContext(newSubstitution: Substitution, calledFrom: Predicate, position: Int, newDepth: Int): Option[ExecutionContext] = {
    val currentHead = getHead()
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


  def newContext(targetAttribute: Variable, restAttributes: Array[Variable]): ExecutionContext =
    new ExecutionContext(rule, dataMap, originalMap, rowMap, originalRowMap, substitution, targetAttribute, relations, restAttributes, depth)

  def newContext(substitution: Substitution): ExecutionContext =
    new ExecutionContext(rule, dataMap, originalMap, rowMap, originalRowMap, substitution, targetVariable, relations, attributes, depth)

  def calledFrom(other: ExecutionContext): Boolean =
    getQuery().calledFrom(other.getQuery())

  def setDataMap(dataMap: Map[Int, Array[Predicate]]): ExecutionContext = {
    this.dataMap = dataMap
    this
  }

  def setRowMap(rowMap: Map[Int, RoaringBitmap]): ExecutionContext = {
    this.rowMap = rowMap
    this
  }

  def setOriginalMap(originalMap: Map[Int, Array[Predicate]]): ExecutionContext = {
    this.originalMap = originalMap
    this
  }

  def getOriginalMap(): Map[Int, Array[Predicate]] = {
    this.originalMap
  }

  def setAttributes(newAttributes: Array[Variable]): ExecutionContext = {
    this.attributes = newAttributes
    this
  }

  def setSubstitution(newSubstitution: Substitution): ExecutionContext = {
    this.substitution = newSubstitution
    this
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
      bitmap.add(Range(0, predicates.size): _*)
      id -> bitmap
    }
    }

    dataMap = dataMap ++ targetMap
    originalMap = originalMap ++ targetMap
    rowMap = rowMap ++ roaringMap
    //originalRowMap = originalRowMap ++ roaringMap
    this

  def updateData(predicate: Predicate, set: Array[Predicate]): ExecutionContext =
    val id = predicate.identifier()
    val targetMap = relations.zipWithIndex.filter { case (relation, position) => relation.identifier() == id }
      .map { case (predicate, index) => {
        predicate.identifier(index) -> set
      }}
      .toMap

    dataMap = dataMap ++ targetMap

    this

  override def toString: String =
    "Rule: " + rule.getQuery().toString + "\n" +
      "Target: " + targetVariable.toString + "\n" +
      "Attributes: " + attributes.mkString("[", ",", "]") + "\n" +
      "Substitution: " + substitution.toString + "\n" +
      "Data size: " + dataMap.map(_._2.size).mkString("[", ",", "]")
  /*

    def getRecursive(targetVariable: Variable, newSubstitution: Substitution): ExecutionContext =
      new ExecutionContext(rule, originalMap, originalMap, originalRowMap,originalRowMap, newSubstitution, targetVariable, relations, attributes, depth + 1)
  */

  inline def emptyAttributes() = attributes.isEmpty

  inline def getRuleId(substitution: Substitution): Int =
    rule.getQueryId()*7 + substitution.id()

  inline def relevant(substitution: Substitution):Boolean=
    rule.getHead().getInput().forall(variable=> substitution.contains(variable))

  def getQuery(): Query = rule.getQuery()

  def getHead(): Predicate = rule.getQuery().getHead()

  def getHeadVariables(): Array[Variable] = rule.getHead().getVariables()

  def getTargetVariable(): Variable = targetVariable

  def getRule(): Optimized = rule

  def getDataMap(): Map[Int, Array[Predicate]] = dataMap
  def getRowMap(): Map[Int, RoaringBitmap] = rowMap
  def getResetDataMap(): Map[Int, Array[Predicate]] = originalMap

  def getSubstitution(): Substitution = substitution

  def getRelations(): Array[Predicate] = relations

  def getAttributes(): Array[Variable] = attributes

  def getDepth(): Int = depth

  def isTarget(): Boolean = rule.getTarget()
  def isFunctional():Boolean = rule.isFunctional()
}

object ExecutionContext {

  def apply(newRule: Optimized, relation: Predicate,
            substitution: Substitution,
            attribute: Variable,
            position: Int, depth: Int): ExecutionContext =
    val newHead = newRule.getHead()
    val newVariable = newHead.getVariable(position)
    val newSubstitution = relation.call(newHead, substitution)
      .composition(newVariable, attribute)
    val newAttributes = newRule.getVariables()
    val newRelations = newRule.getRelations()
    val newMap = newRule.getDataMap()
    val rowMap = newRule.getRoaringMap()
    new ExecutionContext(newRule, newMap, newMap, rowMap, rowMap, newSubstitution, newVariable, newRelations, newAttributes, depth)

  def apply(mainRule: Optimized, substitution: Substitution): ExecutionContext =
    val dataMap = mainRule.getDataMap()
    val rowMap = mainRule.getRoaringMap()
    val relations = mainRule.getRelations()
    val attributes = mainRule.getVariables()
    new ExecutionContext(mainRule, dataMap, dataMap, rowMap, rowMap, substitution, attributes.head, relations, attributes, 0)
}

class Program(programMap: Map[Int, Array[Optimized]])
