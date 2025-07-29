package ilp.data.variables

import ilp.data.Substitution


class VariableList(nm: String, var values: Array[Variable]) extends Variable(nm):

  def this(name: String) = this(name, Array[Variable]())
  def this(name: String, var1: Variable) = this(name, Array(var1))
  def this(name: String, var1: Variable, var2: Variable) = this(name, Array(var1, var2))
  def this(name: String, var1: Variable, var2: Variable, var3: Variable) = this(name, Array(var1, var2, var3))

  def this(name: String, head:Double, items: Array[Double]) = this(name, (head +: items).zipWithIndex.map(pair => Num("item" + pair._2, pair._1).asVariable()))
  def this(name: String, head:String, items: Array[String]) = this(name, (head +: items).zipWithIndex.map(pair => Sym("item" + pair._2, pair._1).asVariable()))

  override def isVariable(): Boolean = true
  override def isVariableList(): Boolean = true
  override def isList(): Boolean = true
  override def isSymbol(): Boolean = true
  override def isNumberList(): Boolean =
    nonEmpty() && values.head.isNumber()
  override def isSymbolList(): Boolean =
    nonEmpty() && values.head.isSymbol()

  override def getSize(): Int = values.size

  def sum(resultName:String):Variable =
    if isSymbolList() then
      val result = values.foldRight[String](""){case(item, main)=>{
        main + item.asSymbol().value
      }}
      Sym(resultName, result)
    else if isNumberList() then
      val result = values.foldRight[Double](0.0) { case (item, main) => {
        main + item.asNumber().getNumber()
      }}
      Num(resultName, result)
    else
      Variable(resultName)


  override def isEmpty(): Boolean = values.isEmpty
  override def substitution(substitution: Substitution): Variable =

    if substitution.hasVariable(this) then {
      val target = substitution.valueByVariable(this).get
      if target.isVariableList() then target.asVariableList()
      else this
    }
    else
      this

  def getArray(): Array[Variable] = values

  def getLength(): Num = Num(name, values.length)

  def getHead(): Variable = values.head

  def getTail(): VariableList = VariableList(name, values.tail)

  def nonEmpty(): Boolean = values.nonEmpty


  def member(variable: Sym): Variable =
    val isFound = values.contains(variable)
    new Sym(variable.name, isFound.toString)

  def append(variable: Sym): Variable =
    val newArray = values :+ variable
    VariableList(name, newArray)

  def prepend(variable: Sym): Variable =
    val newArray = variable +: values
    VariableList(name, newArray)

  override def hashCode(): Int = values.foldRight[Int](name.hashCode()){case(variable, main)=> main*7 + variable.hashCode()}

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Variable] && obj.asInstanceOf[Variable].name == name
  }

  override def equalValue(variable: Variable): Boolean =
    if variable.isVariableList() then
      variable.asVariableList().hashCode() == hashCode()
    else
      false

  override def toString: String = values.mkString("[", ",", "]")

  override def copy(): Variable = VariableList(name, values)
  override def copy(newName:String): Variable = VariableList(newName, values)

