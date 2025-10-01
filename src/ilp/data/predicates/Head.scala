package ilp.data.predicates

import ilp.data.program.Substitution
import ilp.data.variables.{Variable, VariableList}


final class Head(val nm: String, val head: Variable, val list: Variable) extends Functional(nm, Array(head, list)):

  setInput(list)

  def this(head: Variable, list: VariableList) = this("head", head, list)

  override inline def isDefinite: Boolean = {
    list.isVariableList
  }

  override inline def isExecutable: Boolean = {
    list.asVariableList().nonEmpty
  }

  override inline def getValue: Variable = {
    list.asVariableList().getHead.copy(head.getName)
  }

  override def getInput: Array[Variable] = Array(list)

  override inline def copy(): Variable =
    Head(nm, head.copy(), list.copy())

  override inline def copy(varlist: Array[Variable]): Predicate =
    Head(nm, varlist.head, varlist.last)

  override inline def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newList = list.substitution(substitution).asVariableList()
    Head(nm, newHead, newList).asVariable()

  override inline def execute(): Option[Substitution] = {
    if list.isDefinite then Some(Substitution().add(head, getValue))
    else None
  }

  override inline def toString: String = "head(" + head.getName + "," + list.getName + ")"


final class HeadTail(val nm: String, val head: Variable, val tail: Variable, val list: VariableList) extends Predicate(nm, Array(head, tail, list)):

  def this(head: Variable, tail: VariableList, list: VariableList) = this("head_tail", head, tail, list)

  override inline def isDefinite: Boolean = true

  override inline def isExecutable: Boolean = list.nonEmpty

  override inline def getValue: Variable = {
    list.getHead
  }

  override inline def copy(): Variable =
    Head(nm, head.copy(), list.copy().asVariableList())

  override inline def copy(varlist: Array[Variable]): Predicate =
    Head(nm, varlist.head, varlist.last.asVariableList())

  override inline def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()

    HeadTail(nm, newHead, newTail, newList).asVariable()

  override inline def execute(): Option[Substitution] = {
    val h = list.getHead
    val t = list.getTail
    Some(Substitution().add(head, h).add(tail, t))
  }

  override inline def toString: String = nm + "([" + head.getName + "|" + tail.getName + "]," + head.getName + "," + tail.getName + ")"
