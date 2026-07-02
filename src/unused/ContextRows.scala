import ilp.data.{Query, Substitution}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable
import org.roaringbitmap.RoaringBitmap

class ContextRows(var rule: Optimized,
                  var rowMap: Map[Int, RoaringBitmap],
                  var originalRowMap: Map[Int, RoaringBitmap],
                  var substitution: Substitution,
                  val targetVariable: Variable,
                  var relations: Array[Predicate],
                  var attributes: Array[Variable],
                  var depth: Int = 0) extends Serializable{

  def get(substitutions: Set[Substitution]): Set[Predicate] = {
    val headVariables = getHead().getVariables()
    substitutions.map(substitution => rule.getHead().substitution(substitution).asPredicate())
  }

  def hasRecursion(predicate: Predicate): Boolean =
    rule.hasRecursion(predicate)

  def switchContext(newSubstitution: Substitution,calledFrom:Predicate, newDepth:Int): ContextRows = {
    val isolatedSubstitution = newSubstitution.filter(calledFrom)
    new ContextRows(rule, originalRowMap, originalRowMap, isolatedSubstitution, targetVariable, relations, attributes, newDepth)
  }

  def newContext(targetAttribute: Variable, restAttributes: Array[Variable]): ContextRows =
    new ContextRows(rule, rowMap, originalRowMap, substitution, targetAttribute, relations, restAttributes, depth)

  def newContext(substitution: Substitution, targetAttribute: Variable, restAttributes: Array[Variable]): ContextRows =
    new ContextRows(rule, rowMap, originalRowMap, substitution, targetAttribute, relations, restAttributes, depth)

  def newContext(variable: Variable): ContextRows =
    new ContextRows(rule, rowMap, originalRowMap, substitution.composition(variable), targetVariable, relations, attributes, depth)

/*  def calledFrom(other: ContextRows): Boolean =
    getQuery().calledFrom(other.getQuery())*/

  def setRowMap(rowsMap: Map[Int, RoaringBitmap]): ContextRows = {
    this.rowMap = rowsMap
    this
  }

  def setAttributes(newAttributes: Array[Variable]): ContextRows = {
    this.attributes = newAttributes
    this
  }

  def setSubstitution(newSubstitution: Substitution): ContextRows = {
    this.substitution = newSubstitution
    this
  }

  def updateData(predicate: Predicate, array: Set[Predicate]): ContextRows =
    val identifier = predicate.identifier()
    val targetMap = relations.zipWithIndex.filter { case (relation, position) => identifier == relation.identifier() }
      .map { case (predicate, position) => {
        predicate.identifier(position) -> array
      }}.toMap

    val roaringMap = targetMap.map { case (id, predicates) => {
      val bitmap = RoaringBitmap()
      bitmap.add(Range(0, predicates.size): _*)
      id -> bitmap
    }}

    rowMap = rowMap ++ roaringMap
    originalRowMap = originalRowMap ++ roaringMap
    this

  override def toString: String =
    "Rule: " + rule.getQuery().toString + "\n" +
      "Target: " + targetVariable.toString + "\n" +
      "Attributes: " + attributes.mkString("[", ",", "]") + "\n" +
      "Substitution: " + substitution.toString + "\n" +
      "Data size: " + rowMap.map(_._2.toArray.size).mkString("[", ",", "]")

  def emptyAttributes() = attributes.isEmpty

  def getId(): Int = rule.getQueryId()

  def getQuery(): Query = rule.getQuery()

  def getHead(): Predicate = rule.getQuery().getHead()

  def getHeadVariables(): Array[Variable] = rule.getHead().getVariables()

  def getTargetVariable(): Variable = targetVariable

  def getRule(): Optimized = rule

  def getRowMap(): Map[Int, RoaringBitmap] = rowMap
  def getOriginalRowMap(): Map[Int, RoaringBitmap] = originalRowMap
  def getResetDataMap(): Map[Int, Array[Predicate]] = rule.getDataMap()

  def getSubstitution(): Substitution = substitution

  def getRelations(): Array[Predicate] = relations

  def getAttributes(): Array[Variable] = attributes

  def getDepth(): Int = depth

  def isTarget(): Boolean = rule.getTarget()
}

