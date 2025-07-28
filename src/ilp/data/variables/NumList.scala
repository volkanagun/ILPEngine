package ilp.data.variables

import ilp.data.Substitution

class NumList(n:String, var items:Array[Double]) extends Sym(n, items.mkString(",")) :

  def this(name:String) = this(name, Array[Double]())
  def this(name:String, var1:Double) = this(name, Array(var1))
  def this(name:String, var1:Double, var2:Double) = this(name, Array(var1, var2))
  def this(name:String, var1:Double, var2:Double, var3:Double) = this(name, Array(var1, var2, var3))

  override def isNumberList(): Boolean = true
  override def isSymbol(): Boolean = true
  override def getSize():Int = items.size
  override def isEmpty(): Boolean = items.isEmpty
  override def copy(): Variable = NumList(name, items)
  override def copy(name:String): Variable = new NumList(name, items)

  override def substitution(substitution: Substitution): Variable = {
    val targetValue = substitution.valueByVariable(this)
    if targetValue.isDefined && targetValue.get.isNumberList() then
      targetValue.get
    else
      this
  }

  override def id(): Int = items.foldRight(name.hashCode){case(crr, main)=> main * 7 + crr.hashCode()}

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

  override def equalValue(variable: Variable): Boolean =

    if variable.isNumberList() then
      val other = variable.asNumList()
      other.getSize() == items.length && other.items.zip(items).forall(pair => pair._1 == pair._2)
    else
      val otherName = variable.getName()
      otherName == name

  override def equals(compare: Any): Boolean = {
    val variable = compare.asInstanceOf[Variable]
    if variable.isNumberList() then
      val other = variable.asNumList()
      other.getName() == name && other.getSize() == items.length && other.items.zip(items).forall(pair=> pair._1 == pair._2)
    else
      val otherName = variable.getName()
      otherName == name
  }

  override def toString: String = items.mkString("[",",","]")
