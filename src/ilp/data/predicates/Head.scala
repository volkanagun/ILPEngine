package ilp.data.predicates

import ilp.data.Substitution
import ilp.data.variables.{NumList, Variable, VariableList}


class Head(val nm:String, val head: Variable, val list: VariableList) extends Functional(nm, Array(head, list)):

  def this(head:Variable, list:VariableList) = this("head", head, list)

  override def isDefinite(): Boolean =  list.isSymbol()
  override def isExecutable(): Boolean = list.nonEmpty()

  override def getValue(): Variable = {
    list.getHead().copy(head.getName())
  }
  override def copy(): Variable =
    Head(nm, head.copy(), list.copy().asVariableList())

  override def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newList = list.substitution(substitution).asVariableList()
    Head(nm, newHead, newList).asVariable()

  override def execute(): Option[Substitution] =
    Some(Substitution().add(head, getValue()))

  override def toString: String = nm + "(["+head.getName()+"|_],"+head.getName()+")"


class HeadTail(val nm:String, val head: Variable, val tail: Variable, val list: VariableList) extends Predicate(nm, Array(head, tail, list)):

  def this(head:Variable, tail:NumList, list:VariableList) = this("head_tail", head,tail, list)

  override def isDefinite(): Boolean =  true
  override def isExecutable(): Boolean = list.nonEmpty()

  override def getValue(): Variable = {
    list.getHead()
  }
  override def copy(): Variable =
    Head(nm, head.copy(), list.copy().asVariableList())

  override def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()

    HeadTail(nm, newHead, newTail, newList).asVariable()

  override def execute(): Option[Substitution] = {
    val h = list.getHead()
    val t = list.getTail()
    Some(Substitution().add(head, h).add(tail, t))
  }

  override def toString: String = nm + "(["+head.getName()+"|"+tail.getName() + "],"+head.getName()+","+tail.getName()+")"


