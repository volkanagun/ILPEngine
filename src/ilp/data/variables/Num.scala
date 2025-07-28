package ilp.data.variables

class Num(vname: String, var item: Double) extends Sym(vname, item.toString):
  override def isNumber(): Boolean = true

  override def isSymbol(): Boolean = true

  def getNumber(): Double = item

  override def hashCode(): Int = {
    item.hashCode()
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case num: Num => num.item == item
      case variable:Variable => variable.getName() == getName()
      case _ => false
    }

  }

  override def equalValue(variable: Variable): Boolean = variable.isNumber() && variable.asNumber().getNumber() == getNumber()

  override def toString: String = getName() + "=" + value

  override def id(): Int = name.hashCode * 7 + item.hashCode()

  override def copy(): Variable = Num(name, item)
  override def copy(name:String): Variable = new Num(name, item)

  def greater(other: Variable): Boolean =
    item > other.asNumber().item

  def greaterEqual(other: Variable): Boolean =
    item >= other.asNumber().item

  def lower(other: Variable): Boolean =
    item < other.asNumber().item

  def lowerEqual(other: Variable): Boolean =
    item <= other.asNumber().item

  def equal(other: Variable): Boolean =
    item == other.asNumber().item


  def log(): Num =
    Num(name, math.log(item))


