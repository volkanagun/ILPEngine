package ilp.data.variables

import ilp.data.Substitution
import ilp.data.variables.VariableList

class NumList(name:String, var value:Array[Double]) extends Variable(name) :

  def this(name:String) = this(name, Array[Double]())
  def this(name:String, var1:Double) = this(name, Array(var1))
  def this(name:String, var1:Double, var2:Double) = this(name, Array(var1, var2))
  def this(name:String, var1:Double, var2:Double, var3:Double) = this(name, Array(var1, var2, var3))

  override def isNumberList(): Boolean = true
  override def isSymbol(): Boolean = true
  override def getSize():Int = value.size
  override def isEmpty(): Boolean = value.isEmpty
  override def copy(): Variable = NumList(name, value)

  def getNumber():Array[Double] = value
  def getLength():Num = Num(name, value.length)
  def nonEmpty() : Boolean = value.nonEmpty
  def getHead(): Num = Num("X", value.head)
  def getHead(name:String): Num = Num(name, value.head)
  def getTail(): NumList = NumList(name, value.tail)
  def getTail(name:String): NumList = NumList(name, value.tail)

  def average() : Num = Num(name, value.sum/value.length)
  def sum() : Num = Num(name, value.sum)
  def log(): NumList =
    NumList(name, value.map(item => math.log(item)))

  def member(variable:Num):Variable =
    val isFound = value.contains(variable.getNumber())
    new Sym(variable.name, isFound.toString)

  def toVariableList():VariableList =
    VariableList(name, value.map(d=> Num("X", d).asVariable()))

  override def hashCode(): Int = name.hashCode()

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Variable] && obj.asInstanceOf[Variable].name == name

  override def toString: String = value.mkString("[",",","]")