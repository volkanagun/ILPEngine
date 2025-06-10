package ilp.data

import ilp.data.database.Database
import ilp.data.predicates.{Negative, Predicate}
import ilp.data.variables.Variable

class Operation(crrHead: Predicate, crrBody: Array[Predicate], var items: Array[Variable]) extends Query(crrHead, crrBody):

  override def hashCode(): Int =
    var r = head.hashCode()
    r = r * 7 + body.hashCode()
    items.foldRight(r) { case (a, m) => a.hashCode() + m * 7 }

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Operation]
    other.hashCode() == hashCode()

  /*
  override def copy(): Query =
    val definitionClone = head.copy().asPredicate()
    val queryClone = body.map(_.copy().asPredicate())
    val itemsClone = items.map(_.copy())
    Operation(definitionClone, queryClone, itemsClone)*/

  override def toString: String =
    if crrBody.nonEmpty then
      crrHead.toString + " :: " + body.map(_.toString).mkString(" && ") + " ==> " + items.mkString(" && ")
    else
      crrHead.toString + " ==> " + items.mkString(" && ")


  def execute(instance: Predicate): (Option[Substitution], Operation) =
    val call = Substitution().of(crrHead, instance)
    if call.isDefined then
      val main = call.get
      val newFunction = head.substitution(main).asPredicate()
      val newQuery = body.map(variable => variable.substitution(main).asPredicate() /*main.of(variable).asPredicate()*/)
      (call, Operation(newFunction, newQuery, items))
    else
      (None, Operation(head, body, items))

  def execute(headSubstitution: Option[Substitution], substitutions: Set[Substitution]): Operation =
    val applications = if headSubstitution.isDefined && !isAtom() then
      val main = headSubstitution.get
      val newItems = items.map(item => item.substitution(main)  /*head.get.of(item)*/)
      newItems.flatMap(item => substitutions
        .map(crrSubstitution => item.substitution(crrSubstitution) /*substitution.of(item)*/))
    else if headSubstitution.isDefined && isAtom() then
      val main = headSubstitution.get
      items.map(item => item.substitution(main) /*head.get.of(item)*/)
    else
      items.flatMap(item => substitutions
        .map(substitution => item.substitution(substitution) /*substitution.of(item)*/))

    Operation(head, body, applications)


