package ilp.data.database

import ilp.data.{Query, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.{Num, Variable}

class ContextData(var rule: Optimized,
                  var dataMap: Map[Int, Set[Predicate]],
                  var originalMap: Map[Int, Set[Predicate]],
                  var substitution: Substitution,
                  val targetVariable: Variable,
                  var relations: Array[Predicate],
                  var attributes: Array[Variable],
                  var depth: Int = 0) {

  def get(substitutions: Set[Substitution]): Set[Predicate] = {
    val headVariables = getHead().getVariables()
    substitutions
      .map(substitution => rule.getHead().substitution(substitution).asPredicate())
  }

  def canSwitchContext(substitution: Substitution): Boolean =
    getQuery().getInputVariables().forall(variable => substitution.hasVariable(variable))

  def conflictContext(newContext: ContextData): Boolean = {
    val result = depth == newContext.getDepth() && substitution.conflicts(newContext.getSubstitution())
    if result then {
      true
    }
    else false
  }

  def switchContext(newSubstitution: Substitution, calledFrom: Predicate, position: Int, newDepth: Int): Option[ContextData] = {
    val currentHead = getHead()
    val switchedSubstitution = calledFrom.call(currentHead, newSubstitution)
    if canSwitchContext(switchedSubstitution) then
      Some(new ContextData(rule, originalMap, originalMap, switchedSubstitution, targetVariable, relations, attributes, newDepth))
    else
      None
  }

  def nextContext(newSubstitution: Substitution): ContextData =
    val restAttributes = newSubstitution.compose(attributes.tail)
    val nextAttribute = newSubstitution.compose(attributes.head)
    new ContextData(rule, dataMap, originalMap, newSubstitution, nextAttribute, relations, restAttributes, depth)


  def newContext(targetAttribute: Variable, restAttributes: Array[Variable]): ContextData =
    new ContextData(rule, dataMap, originalMap, substitution, targetAttribute, relations, restAttributes, depth)

  def newContext(substitution: Substitution): ContextData =
    new ContextData(rule, dataMap, originalMap, substitution, targetVariable, relations, attributes, depth)

  def calledFrom(other: ContextData): Boolean =
    getQuery().calledFrom(other.getQuery())

  def setDataMap(dataMap: Map[Int, Set[Predicate]]): ContextData = {
    this.dataMap = dataMap
    this
  }

  def setOriginalMap(originalMap: Map[Int, Set[Predicate]]): ContextData = {
    this.originalMap = originalMap
    this
  }

  def getOriginalMap(): Map[Int, Set[Predicate]] = {
    this.originalMap
  }

  def setAttributes(newAttributes: Array[Variable]): ContextData = {
    this.attributes = newAttributes
    this
  }

  def setSubstitution(newSubstitution: Substitution): ContextData = {
    this.substitution = newSubstitution
    this
  }


  def updateData(predicate: Predicate, set: Set[Predicate]): ContextData =
    val id = predicate.identifier()
    val targetMap = relations.zipWithIndex.filter { case (relation, position) => relation.identifier() == predicate.identifier() }
      .map { case (predicate, index) => {
        predicate.identifier(index) -> set
      }
      }
      .toMap
    dataMap = dataMap ++ targetMap
    this

  override def toString: String =
    "Rule: " + rule.getQuery().toString + "\n" +
      "Target: " + targetVariable.toString + "\n" +
      "Attributes: " + attributes.mkString("[", ",", "]") + "\n" +
      "Substitution: " + substitution.toString + "\n" +
      "Data size: " + dataMap.map(_._2.size).mkString("[", ",", "]")

  def getRecursive(targetVariable: Variable, newSubstitution: Substitution): ContextData =
    new ContextData(rule, originalMap, originalMap, newSubstitution, targetVariable, relations, attributes, depth + 1)

  def emptyAttributes() = attributes.isEmpty

  def getRuleId(): Int = rule.getQueryId()

  def getQuery(): Query = rule.getQuery()

  def getHead(): Predicate = rule.getQuery().getHead()

  def getHeadVariables(): Array[Variable] = rule.getHead().getVariables()

  def getTargetVariable(): Variable = targetVariable

  def getRule(): Optimized = rule

  def getDataMap(): Map[Int, Set[Predicate]] = dataMap

  def getResetDataMap(): Map[Int, Set[Predicate]] = rule.getDataMap()

  def getSubstitution(): Substitution = substitution

  def getRelations(): Array[Predicate] = relations

  def getAttributes(): Array[Variable] = attributes

  def getDepth(): Int = depth

  def isTarget(): Boolean = rule.getTarget()
}

object ContextData {
  def apply(newRule: Optimized, relation: Predicate, substitution: Substitution,
            attribute: Variable,
            position: Int, depth: Int): ContextData =
    val newHead = newRule.getHead()
    val newVariable = newHead.getVariable(position)
    val newSubstitution = relation.call(newHead, substitution)
      .composition(newVariable, attribute)
    val newAttributes = newRule.getVariables()
    val newRelations = newRule.getRelations()
    val newMap = newRule.getDataMap()
    new ContextData(newRule, newMap, newMap, newSubstitution, newVariable, newRelations, newAttributes, depth)

  def apply(mainRule: Optimized, substitution: Substitution): ContextData =
    val dataMap = mainRule.getDataMap()
    val relations = mainRule.getRelations()
    val attributes = mainRule.getVariables()
    new ContextData(mainRule, dataMap, dataMap, substitution, attributes.head, relations, attributes, 0)
}

class Program(programMap: Map[Int, Array[Optimized]])
