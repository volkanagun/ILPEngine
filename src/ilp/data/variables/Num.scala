package ilp.data.variables

class Num(name:String, var value:Double) extends Variable(name) :
  override def isNumber(): Boolean = true
  override def isSymbol(): Boolean = true

  def getNumber():Double = value
  override def hashCode(): Int = value.hashCode()
  override def equals(obj: Any): Boolean = obj.isInstanceOf[Num] && obj.asInstanceOf[Num].value == value

  override def toString: String = value.toString

  def greater(other:Variable):Boolean =
    value > other.asNumber().value

  def lower(other:Variable):Boolean =
    value < other.asNumber().value

  def equal(other:Variable):Boolean =
    value == other.asNumber().value

  def log():Num =
    Num(name, math.log(value))


