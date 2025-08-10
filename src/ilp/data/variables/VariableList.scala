package ilp.data.variables

import ilp.data.Substitution


final class VariableList(nm: String, var values: Array[Variable]) extends Variable(nm):

  val empty = values.isEmpty
  val containsNumber = nonEmpty() && values.head.isNumber()
  val containsSymbol = nonEmpty() && values.head.isSymbol()
  val size = values.length


  def this(name: String) = this(name, Array[Variable]())

  def this(name: String, var1: Variable) = this(name, Array(var1))

  def this(name: String, var1: Variable, var2: Variable) = this(name, Array(var1, var2))

  def this(name: String, var1: Variable, var2: Variable, var3: Variable) = this(name, Array(var1, var2, var3))

  def this(name: String, head: Double, items: Array[Double]) = this(name, (head +: items).zipWithIndex.map(pair => Num("item" + pair._2, pair._1).asVariable()))

  def this(name: String, head: String, items: Array[String]) = this(name, (head +: items).zipWithIndex.map(pair => Sym("item" + pair._2, pair._1).asVariable()))

  override inline def isVariable(): Boolean = true

  override inline def isVariableList(): Boolean = true

  override inline def isList(): Boolean = true

  override inline def isSymbol(): Boolean = true

  override inline def isNumberList(): Boolean =
    containsNumber



  inline def reverse(): VariableList =
    VariableList(name, values.reverse)

  override inline def isSymbolList(): Boolean =
    containsSymbol

  override inline def getSize(): Int = size

  inline def sum(resultName: String): Variable =
    if containsSymbol then
      val result = values.foldRight[String]("") { case (item, main) => {
        main + item.asSymbol().value
      }
      }
      Sym(resultName, result)
    else if containsNumber then
      val result = values.foldRight[Double](0.0) { case (item, main) => {
        main + item.asNumber().getNumber()
      }
      }
      Num(resultName, result)
    else
      Variable(resultName)

  inline def avg(resultName: String): Variable =
    if containsNumber then
      val result = values.foldRight[Double](0.0) { case (item, main) => {
        main + item.asNumber().getNumber()
      }
      }
      Num(resultName, result / values.size)
    else
      Num(resultName, 0.0)


  override inline def isEmpty(): Boolean = empty

  override def substitution(substitution: Substitution): Variable =
    if substitution.hasVariable(this) then {
      val target = substitution.valueByVariable(this).get
      //Modify in the Future, be careful!!!
      if target.isVariableList() then target.copy(getName())
      else if target.isVariable() then target.toVariableList()
      else this
    }
    else
      this


  inline def getHead(): Variable = values.head

  inline def getTail(): VariableList = VariableList(name, values.tail)

  inline def nonEmpty(): Boolean = !empty

  inline def member(variable: Variable): Variable =
    val isFound = values.contains(variable)
    new Sym(variable.name, isFound.toString)

  inline def append(variable: Variable): Variable =
    val newArray = values :+ variable
    VariableList(name, newArray)

  inline def prepend(variable: Variable): Variable =
    val newArray = variable +: values
    VariableList(name, newArray)

  override inline def hashCode(): Int = values.foldRight[Int](name.hashCode()) { case (variable, main) => main * 7 + variable.hashCode() }

  override inline def equals(obj: Any): Boolean = {

    obj match {
      case list:VariableList => {list.hashCode() == hashCode()}
      case num: Num => false
      case sym: Sym => false
      case num: Variable => num.name.equals(name)

    }

  }

  override inline def equalValue(variable: Variable): Boolean =
    if variable.isVariableList() then
      variable.asVariableList().hashCode() == hashCode()
    else
      false

  override inline def toString: String = {
    name+"="+values.mkString("[",",","]")
  }

  override inline def copy(): Variable = VariableList(name, values)

  override inline def copy(newName: String): Variable = VariableList(newName, values)

