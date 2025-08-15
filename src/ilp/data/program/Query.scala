package ilp.data.program

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Query(var head: Predicate, var body: Array[Predicate]) extends Serializable:

  var recursive = false
  var functional: Boolean = body.exists(_.isFunctional)
  var inputVariables: Array[Variable] = body.flatMap(predicate=> predicate.getInput)

  head.setFunctional(functional)

  inline def getAttributes: Set[Variable] =
    body.flatMap(predicate => predicate.getVariables)
      .toSet

  inline def getAttributeArray: Array[Variable] =
    body.flatMap(predicate => predicate.getVariables).distinct

  inline def getNonRecursive: Query =
    Query(head, body.filter(predicate => !predicate.equalByIdentifier(head)))

  inline def getNonRecursiveBody: Array[Predicate] =
    body.filter(predicate => !predicate.equalByIdentifier(head))

  inline def setInputVariables(variables:Array[Variable]):this.type = {
    this.inputVariables = variables
    this
  }

  inline def getInputVariables:Array[Variable] = {
    this.inputVariables
  }

  inline def renameHead(name: String): Query = {
    val newQuery = Query(head.copy(name), body)
    newQuery
  }

  inline def setHead(head: Predicate): this.type = {
    this.head = head
    this
  }

  inline def setRecursive(recursive: Boolean): this.type =
    this.recursive = recursive
    this

  inline def setFunctional(functional: Boolean): this.type =
    this.functional = functional
    this.head.setFunctional(functional)
    this

/*  inline def doRecursion(item: Predicate): Boolean =
    item.identifier() == head.identifier() && !item.isEmpty*/

  inline def isAtom: Boolean = body.isEmpty
  inline def isDefinite: Boolean = head.isDefinite
  inline def getBody: Array[Predicate] = body
  inline def getSortedBody: Array[Predicate] = body.sortBy(_.getName)

  inline def isRecursive: Boolean = recursive
  inline def isFunctional: Boolean = functional
  inline def identifier(): Int = head.identifier()

  inline def asRule(): Rule =
    asInstanceOf[Rule]

  inline def toRule: Rule =
    Rule(head, body)
      .setRecursive(recursive)
      .setFunctional(functional)
      .setInputVariables(inputVariables)


  inline def callByVariable(predicate: Predicate): Query = {
    val variables = head.getVariables
    val symbols = predicate.getVariables
    val substitution = Substitution(variables,symbols)
    call(substitution)
  }

  inline def call(substitution: Substitution): Query =
    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(item => item.substitution(substitution).asPredicate())
    Query(newHead, newBody)
      .setRecursive(recursive)


  inline def contains(predicate: Predicate): Boolean =
    body.contains(predicate)

  inline def calledFrom(otherRule: Query): Boolean = {
    val head = getHead
    otherRule.getBody.exists(otherPredicate => head.equalByIdentifier(otherPredicate))
  }

  inline def calls(otherRule: Query): Boolean = {
    val predicate = otherRule.getHead
    body.exists(bodyPredicate => predicate.equalByIdentifier(bodyPredicate))
  }

  inline def getAritry: Int =
    head.getArity


  inline def getHead: Predicate =
    head

  inline def getHeadName: String =
    head.getName

  def getHeadIdentifier: Int =
    head.identifier()

  override def hashCode(): Int =
    getSortedBody.foldRight(head.hashCode()) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Query =>
        other.head.equals(head) && other.getBody.forall(predicate => contains(predicate)) &&
          getBody.forall(predicate => other.contains(predicate))
      case _ => false
    }

  override def toString: String =
    if body.nonEmpty then head.toString + " :- " + body.map(_.toString).mkString(" & ") + "."
    else head.toString + "."
