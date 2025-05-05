package ilp.data.variables

import ilp.data.predicates.Predicate

class Num(name:String, var item:Double) extends Sym(name, item.toString) :
  override def isNumber(): Boolean = true
  override def isSymbol(): Boolean = true

  def getNumber():Double = item
  override def hashCode(): Int = item.hashCode()
  override def equals(obj: Any): Boolean = obj.isInstanceOf[Num] && obj.asInstanceOf[Num].item == item
  override def equalValue(variable: Variable): Boolean = variable.isNumber() && variable.asNumber().getNumber() == getNumber()

  override def toString: String = value

  override def copy(): Variable = Num(name, item)

  def greater(other:Variable):Boolean =
    item > other.asNumber().item

  def greaterEqual(other:Variable):Boolean =
    item >= other.asNumber().item

  def lower(other:Variable):Boolean =
    item < other.asNumber().item

  def lowerEqual(other:Variable):Boolean =
    item <= other.asNumber().item

  def equal(other:Variable):Boolean =
    item == other.asNumber().item



  def log():Num =
    Num(name, math.log(item))


