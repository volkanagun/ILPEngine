package ilp.data.variables

class NumList(name:String, var items:Array[Double]) extends Sym(name, items.mkString(",")) :

  def this(name:String) = this(name, Array[Double]())
  def this(name:String, var1:Double) = this(name, Array(var1))
  def this(name:String, var1:Double, var2:Double) = this(name, Array(var1, var2))
  def this(name:String, var1:Double, var2:Double, var3:Double) = this(name, Array(var1, var2, var3))

  override def isNumberList(): Boolean = true
  override def isSymbol(): Boolean = true
  override def getSize():Int = items.size
  override def isEmpty(): Boolean = items.isEmpty
  override def copy(): Variable = NumList(name, items)


  def nonEmpty() : Boolean = items.nonEmpty
  def getHead(): Num = Num("X", items.head)
  def append(num:Num): NumList = NumList(name, items:+num.getNumber())
  def prepend(num:Num): NumList = NumList(name, num.getNumber() +: items)
  def reverse(): NumList = NumList(name, items.reverse)

  //def getHead(name:String): Num = Num(name, value.head)
  def getTail(): NumList = NumList(name, items.tail)
  def getTail(name:String): NumList = NumList(name, items.tail)

  def average() : Num = Num(name, items.sum/items.length)
  def sum() : Num = Num(name, items.sum)
  def log(): NumList =
    NumList(name, items.map(item => math.log(item)))

  override def hashCode(): Int = name.hashCode()
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Variable] && obj.asInstanceOf[Variable].name == name
  override def toString: String = items.mkString("[",",","]")
