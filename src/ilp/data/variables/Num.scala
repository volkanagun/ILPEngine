package ilp.data.variables

final class Num(vname: String, val item: Double) extends Variable(vname):
  override inline def isNumber: Boolean = true
  override inline def isSymbol: Boolean = true
  inline def getNumber: Double = item

  override inline def hashCode(): Int = {
    item.hashCode()
  }

  override inline def equals(obj: Any): Boolean = {
    obj match {
      case num: Num => num.item == item
      case num: Sym => false
      case num: VariableList => false
      case variable:Variable => variable.getName == getName
      case _ => false
    }

  }

  override inline def equalValue(variable: Variable): Boolean = variable.isNumber && variable.asNumber().getNumber == getNumber

  override inline def toString: String = getName + "=" + item.toString

  override inline def id(): Int = name.hashCode * 7 + item.hashCode()

  override inline def copy(): Variable = Num(name, item)
  override inline def copy(name:String): Variable = new Num(name, item)

  inline def greater(other: Num): Boolean =
    item > other.item

  inline def greaterEqual(other: Num): Boolean =
    item >= other.item

  inline def lower(other: Num): Boolean =
    item < other.item

  inline def lowerEqual(other: Num): Boolean =
    item <= other.item

/*  inline def equal(other: Variable): Boolean =
    item == other.asNumber().item*/

/*  inline def log(): Num =
    Num(name, math.log(item))*/


