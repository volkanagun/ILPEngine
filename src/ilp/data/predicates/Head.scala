package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{ Variable, VariableList}


final class Head(val nm:String, val head: Variable, val list: VariableList) extends Functional(nm, Array(head, list)):

  setInput(Array(list))

  def this(head:Variable, list:VariableList) = this("head", head, list)

  override inline def isDefinite(): Boolean =  list.isSymbol()
  override inline def isExecutable(): Boolean = list.nonEmpty()

  override inline def getValue(): Variable = {
    list.getHead().copy(head.getName())
  }

  override inline def getInput(): Array[Variable] = Array(list)

  override inline def copy(): Variable =
    Head(nm, head.copy(), list.copy().asVariableList())

  override inline def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newList = list.substitution(substitution).asVariableList()
    Head(nm, newHead, newList).asVariable()

  override inline def execute(): Option[Substitution] =
    Some(Substitution().add(head, getValue()))

  override inline def toString: String = "head("+head.getName()+","+list.getName()+")"


final class HeadTail(val nm:String, val head: Variable, val tail: Variable, val list: VariableList) extends Predicate(nm, Array(head, tail, list)):

  def this(head:Variable, tail:VariableList, list:VariableList) = this("head_tail", head,tail, list)

  override inline def isDefinite(): Boolean =  true
  override inline def isExecutable(): Boolean = list.nonEmpty()

  override inline def getValue(): Variable = {
    list.getHead()
  }
  override inline def copy(): Variable =
    Head(nm, head.copy(), list.copy().asVariableList())

  override inline def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()

    HeadTail(nm, newHead, newTail, newList).asVariable()

  override inline def execute(): Option[Substitution] = {
    val h = list.getHead()
    val t = list.getTail()
    Some(Substitution().add(head, h).add(tail, t))
  }

  override inline def toString: String = nm + "(["+head.getName()+"|"+tail.getName() + "],"+head.getName()+","+tail.getName()+")"
