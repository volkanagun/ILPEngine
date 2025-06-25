package ilp.data.database

import ilp.data.{Query, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Context(var rule:Optimized,
              var dataMap : Map[Int, Set[Predicate]],
              var substitution: Substitution,
              val targetVariable:Variable,
              var relations : Array[Predicate],
              var attributes:Array[Variable],
              var depth:Int = 0) {

  def get(substitutions: Set[Substitution]) : Set[Predicate] = {
    val headVariables = getHead().getVariables()
    substitutions
      .map(substitution=> rule.getHead().substitution(substitution).asPredicate())
  }


  def newContext(targetAttribute:Variable, restAttributes:Array[Variable]):Context=
    new Context(rule, dataMap, substitution, targetAttribute, relations, restAttributes, depth)

  def newContext(substitution: Substitution, targetAttribute:Variable, restAttributes:Array[Variable]):Context=
    new Context(rule, dataMap, substitution, targetAttribute, relations, restAttributes, depth)



  def calledFrom(other: Context):Boolean =
    getQuery().calledFrom(other.getQuery())

  def setDataMap(dataMap: Map[Int, Set[Predicate]]): Context = {
    this.dataMap = dataMap
    this
  }

  def setAttributes(newAttributes:Array[Variable]):Context = {
    this.attributes = newAttributes
    this
  }
  def setSubstitution(newSubstitution:Substitution):Context = {
    this.substitution = newSubstitution
    this
  }



  def updateData(predicate:Predicate, set:Set[Predicate]):Context =
    val id = predicate.identifier()
    val targetMap = relations.zipWithIndex.filter{case(relation, position) => relation.identifier() == predicate.identifier()}
      .map{case(predicate, index)=>{predicate.identifier(index)-> set}}
      .toMap
    dataMap = dataMap ++ targetMap
    this

  override def toString: String =
    "Rule: " + rule.getQuery().toString + "\n" +
    "Target: " + targetVariable.toString + "\n" +
    "Attributes: " + attributes.mkString("[",",","]") + "\n" +
    "Substitution: "+substitution.toString + "\n" +
    "Data size: " + dataMap.map(_._2.size).mkString("[",",","]")

  def emptyAttibutes() = attributes.isEmpty
  def getId() = rule.queryId()
  def getQuery():Query = rule.getQuery()
  def getHead():Predicate = rule.getQuery().getHead()
  def getHeadVariables() = rule.getHead().getVariables()
  def getTargetVariable()=targetVariable
  def getRule() = rule
  def getDataMap() = dataMap
  def getResetDataMap() = rule.getDataMap()
  def getSubstitution() = substitution
  def getRelations() = relations
  def getAttributes() = attributes
  def getDepth() = depth
  def isTarget() = rule.getTarget()
}

object Context {
  def apply(newRule: Optimized, relation: Predicate, substitution: Substitution,
            attribute:Variable,
            position: Int, depth: Int):Context =
    val newHead = newRule.getHead()
    val newVariable = newHead.getVariable(position)
    val newSubstitution = relation.call(newHead, substitution)
      .composition(newVariable, attribute)
    val newAttributes = newRule.getVariables()
    val newRelations = newRule.getRelations()
    val newMap = newRule.getDataMap()
    new Context(newRule, newMap, newSubstitution, newVariable, newRelations, newAttributes, depth)

  def apply(mainRule:Optimized, substitution: Substitution):Context=
    val dataMap = mainRule.getDataMap()
    val relations = mainRule.getRelations()
    val attributes = mainRule.getVariables()
    new Context(mainRule, dataMap, substitution, attributes.head, relations, attributes, 0)
}

class Program(programMap:Map[Int, Array[Optimized]])
