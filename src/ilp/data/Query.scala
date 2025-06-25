package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Query(var head: Predicate, var body: Array[Predicate]):

  var recursive = false
  var positions = Map[Position, Set[Position]]()
  var positionsJoin = Array[(String, Array[Position])]()

  def getAttributes(): Set[Variable] =
    body.flatMap(predicate => predicate.getVariables())
      .toSet

  def getNonRecursive(): Query =
    Query(head, body.filter(predicate => !predicate.equalByIdentifier(head)))


  def renameHead(name: String): Query = {
    val newQuery = Query(head.copy(name), body)
    newQuery
  }

  def setHead(head: Predicate): this.type = {
    this.head = head
    this
  }

  def setRecursive(recursive: Boolean): this.type =
    this.recursive = recursive
    this

  def doRecursion(item: Predicate): Boolean =
    item.identifier() == head.identifier() && !item.isEmpty()

  def isAtom(): Boolean = body.isEmpty

  def isDefinite(): Boolean = head.isDefinite()

  def getBody(): Array[Predicate] = body
  def getSortedBody(): Array[Predicate] = body.sortBy(_.getName())

  def getBodyId() = getSortedBody().foldRight[Int](1){case(p, main) => main * 7 + p.hashCode()}

  def isRecursive(): Boolean = recursive

  def isComplete(): Boolean =
    val set = Set(head) ++ body
    val zipped = set.zipWithIndex
    !zipped.exists { case (predicate, index) => {
      val others = zipped.filter(other => other._2 != index)
        .flatMap(pair => pair._1.getVariables())
      predicate.getVariables().exists(variable => !others.contains(variable))
    }
    }

  def identifier(): Int = head.identifier()

  def asRule(): Rule =
    asInstanceOf[Rule]

  def toRule(): Rule =
    Rule(head, body)


  def callByVariable(predicate: Predicate): Query = {
    val variables = head.getVariables()
    val symbols = predicate.getVariables()
    val substitution = Substitution(variables,symbols)
    call(substitution)
  }


  def callBySymbol(predicate: Predicate): Query = {
    val new_variables = predicate.getVariables()
    val crr_variables = head.getVariables()
    val substitution = Substitution()
    for i <- 0 until new_variables.length do
      val variable = crr_variables(i)
      val symbol = new_variables(i)
        .copy(variable.getName())
      substitution.add(variable, symbol)

    call(substitution)
  }

  def call(substitution: Substitution): Query =
    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(item => item.substitution(substitution).asPredicate())
    Query(newHead, newBody)
      .setRecursive(recursive)


  def contains(predicate: Predicate): Boolean =
    body.contains(predicate)

  def containsByIdentifier(predicate: Predicate): Boolean =
    body.exists(p => p.equalByIdentifier(predicate))


  def calledFrom(otherRule: Query): Boolean =
    otherRule.getBody().exists(otherPredicate => otherPredicate.identifier() == identifier())

  def calls(otherRule: Query): Boolean = {
    val predicate = otherRule.getHead()
    body.exists(bodyPredicate => predicate.equalByIdentifier(bodyPredicate))
  }

  def getAritry(): Int =
    head.getArity()


  def getHead(): Predicate =
    head

  def getHeadIdentifier(): Int =
    head.identifier()

  override def hashCode(): Int =
    getSortedBody().foldRight(head.hashCode()) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Query] then
      val other = obj.asInstanceOf[Query]
      other.head.equals(head) && other.getBody().forall(predicate => contains(predicate)) &&
        getBody().forall(predicate => other.contains(predicate))
    else
      false

  override def toString: String =
    if body.nonEmpty then head.toString + " :- " + body.map(_.toString).mkString(" & ") + "."
    else head.toString + "."
