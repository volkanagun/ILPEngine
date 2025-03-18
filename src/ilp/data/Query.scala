package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

import scala.collection.parallel.CollectionConverters.SetIsParallelizable

class Query(var head: Predicate, var body: Array[Predicate]):
  var recursive = false
  var positions = Map[Position, Set[Position]]()
  var positionsBody = Array[(String, Array[Position])]()


  def compile(): this.type =
    val allPositions = head.getPositions(-1) ++ body.zipWithIndex.flatMap{case(p, pindex) =>p.getPositions(pindex)}
    positions = allPositions.map(crr=> (crr -> allPositions.filter(other=> crr.equalsByName(other)).toSet))
      .toMap

    val crrPositions = body.zipWithIndex.flatMap{case(p, pindex)=>p.getPositions(pindex)}
    positionsBody = crrPositions.groupBy(p=> p.getVariable().getName()).toArray
      .sortBy{case(_, array) => array.map(p=> p.pindex).sum}

    this

  def setRecursive(recursive: Boolean): this.type =
    this.recursive = recursive
    this

  def doRecursion(item: Predicate): Boolean =
    item.identifier() == head.identifier() && !item.isEmpty()

  //def isAtom(): Boolean = body.size == 1 && body.head.isDefinite()
  def isAtom(): Boolean = body.isEmpty

  def isDefinite(): Boolean = head.isDefinite()

  def isNegation(): Boolean = body.size == 1 && body.head.isNegative()

  def getBody(): Array[Predicate] = body
  def getPositions(): Map[Position, Set[Position]] = positions
  def getBodyPosition(): Array[(String, Array[Position])] = positionsBody

  def isRecursive(): Boolean = recursive

  def nonRecursive(): Boolean = !recursive

  def isComplete(): Boolean =
    val set = Set(head) ++ body
    val zipped = set.zipWithIndex
    !zipped.exists { case (predicate, index) => {
      val others = zipped.filter(other => other._2 != index)
        .flatMap(pair => pair._1.getVariables())
      predicate.getVariables().exists(variable => !others.contains(variable))
    }
    }

  //def isList():Boolean = head.isList()
  //def isEmptyList():Boolean = head.isList()
  def identifier(): Int = head.identifier()


  def asRule(): Rule =
    asInstanceOf[Rule]

  def call(predicate: Predicate): Query = {
    val new_variables = predicate.array
    val crr_variables = head.array
    val substitution = Substitution(crr_variables, new_variables)
    call(substitution).compile()
  }

  def call(substitution: Substitution): Query =
    val newHead = head.substitution(substitution).asPredicate()
    val newBody = body.map(item => item.substitution(substitution).asPredicate())
    Query(newHead, newBody).compile()


  def addCopy(predicate: Predicate): Query =
    Query(head, body :+ predicate)
      .compile()

  /*def replace(index:Int, newHead:Predicate, predicates:Array[Predicate]):Query =
    var newBody = Array[Predicate]()
    body.zipWithIndex.foreach(pair=>{
      if pair._2 == index then
        newBody ++= predicates.map(p=> {
          if p.name == newHead.name then p.setName(head.name)
          else p
        })
      else
        newBody :+= pair._1
    })

    Query(head, newBody)*/

  def contains(predicate: Predicate): Boolean =
    body.contains(predicate)

  def containsByIdentifier(predicate: Predicate): Boolean =
    body.exists(p => p.equalByIdentifier(predicate))


  def calledFrom(otherRule: Query): Boolean =
    otherRule.getBody().exists(otherPredicate => otherPredicate.identifier() == identifier())

  def getAritry(): Int =
    head.getArity()

  def getHead(): Predicate =
    head

  def getAbstractName(): String =
    body.map(p => p.getName()).mkString("_")

  /*
  def expandCall(rule: Hypothesis): Rule =
    var newBody = Array[Predicate]()
    for target <- body do
      if target.identifier() == rule.identifier() then
        newBody ++= rule.call(target).getBody()
      else
        newBody :+= target

    Rule(head, newBody)*/

/*  def expandCall(rules: Set[Hypothesis]): Rule =
    var newBody = Array[Predicate]()
    for target <- body do
      for rule <- rules do
        if target.identifier() == rule.identifier() then
          newBody ++= rule.call(target).getBody()
        else
          newBody :+= target


    Rule(head, newBody)*/

  /*def addPredicate(predicate: Predicate): Boolean =
    var r = false
    if !body.contains(predicate) then
      body :+= predicate
      r = true

    r*/

  def add(predicate: Predicate): this.type =
    if !body.contains(predicate) then
      body :+= predicate
    this

  override def hashCode(): Int =
    body.foldRight(head.hashCode()) { case (a, m) => a.hashCode() + 7 * m }

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

  def copy(): Query =
    Query(head.copy().asPredicate(), body.map(_.copy().asPredicate()))

  def boundHead(): Set[Variable] =
    head.array.filter(variable => body.exists(predicate => predicate.contains(variable)))
      .toSet

  def boundBody(): Set[Variable] =
    val variables = body.map(predicate => predicate.getVariables())
    body.zipWithIndex.flatMap { case (predicate, index) => {
      val otherVariables = body.zipWithIndex.filter(_._2 != index).flatMap(_._1.getVariables()).toSet
      predicate.getVariables().filter(variable => otherVariables.contains(variable))
    }
    }.toSet

  def boundPosition(): Set[(Int, Set[Position])] =
    val variables = body.map(predicate => predicate.getVariables())
    body.zipWithIndex.map { case (predicate, pindex) => {
      val otherVariables = body.zipWithIndex.filter(_._2 != pindex).flatMap(_._1.getVariables()).toSet
      (pindex, predicate.getVariables().zipWithIndex.filter { case (variable, position) => otherVariables.contains(variable) }
        .map(pair => Position(predicate,pindex, pair._2)).toSet)
    }
    }.toSet

  def unboundHead(): Set[Variable] =
    head.array.filter(variable => !body.exists(predicate => predicate.contains(variable)))
      .toSet

  def unboundBody(): Set[Variable] =
    body.flatMap(predicate => predicate.array)
      .filter(variable => !head.contains(variable)).toSet

  def unboundAll(): Set[Variable] =
    val set = Set(head) ++ body
    set.map(predicate => (predicate, set.filter(!_.equals(predicate))))
      .flatMap { case (predicate, others) => {
        predicate.array.filter(variable => !others.exists(other => other.contains(variable)))
      }
      }


  def unboundBodyPositions(): Set[Position] =
    val set = body
    set.zipWithIndex.map{case(predicate, pindex) => (predicate, pindex, set.filter(!_.equals(predicate)))}
      .flatMap { case (predicate, pindex, others) => {
        predicate.array.zipWithIndex
          .filter { case (variable, index) => !others.exists(other => other.contains(variable)) }
          .map { case (_, index) => Position(predicate,pindex, index) }
      }
      }.toSet

  def unboundHeadPositions(): Set[Position] =
    body.zipWithIndex.flatMap { case(predicate, pindex) => {
      predicate.getArray().zipWithIndex.filter { case (variable, index) => !head.contains(variable) }
        .map { case (variable, index) => {
          Position(predicate, pindex, index)
        }
        }
    }}.toSet

