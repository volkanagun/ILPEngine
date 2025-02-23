package ilp.data

import ilp.data.variables.{Collection, NumList, Variable, VariableList}


class Predicate(crr_name: String, var array: Array[Variable]) extends Variable(crr_name):

  def this(name: String, item1: Variable) = this(name, Array(item1))

  def this(name: String, item1: Variable, item2: Variable) = this(name, Array(item1, item2))

  def this(name: String, item1: Variable, item2: Variable, item3: Variable) = this(name, Array(item1, item2, item3))

  var collection: Collection = null

  def setCollection(collection: Collection): this.type =
    this.collection = collection
    this

  def getArray(): Array[Variable] =
    this.array

  def getArity(): Int =
    this.array.length

  override def getValue(): Variable = this

  def getVariable(index: Int): Variable =
    array(index)

  def getSymbol(index: Int): variables.Sym =
    array(index).asSymbol()

  def getVariables(): Array[Variable] =
    array.map(_.asVariable())

  def getSymbols(): Array[variables.Sym] =
    array.map(_.asSymbol())

  def getLiterals(): Array[String] =
    array.map(item => item.name) ++
      array.filter(_.isSymbol()).map(_.asInstanceOf[variables.Sym].value)

  def getPositions(): Array[Position] =
    (0 until length()).map(index => Position(this, index))
      .toArray

  def bindTo(predicate: Predicate): Predicate =
    val otherVars = predicate.array.filter(_.isVariable())
    Predicate(name, otherVars)

  def bindTo(elements: Array[Variable]): Predicate =
    Predicate(name, elements)

  def getReplace(names: Array[String]): Substitution =
    val vars = array.map(item => Variable(item.name))
    val reps = names.map(name => Variable(name))
    Substitution(vars, reps)

  override def getComplexity(): Double =
    val symbolComplexity = array.foldRight(0.0) { case (s, m) => s.getComplexity() + m }
    if isNegative() then 2d * symbolComplexity
    else symbolComplexity

  override def substitution(substitution: Substitution): Variable =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newArray = array.map(variable => variable.substitution(substitution))
    Predicate(newName, newArray)

  def toPredicate(newName: String): Predicate =
    Predicate(newName, array.map(_.copy()))


  def toGeneric(): Predicate =
    Predicate(name, array.map(item => Variable(item.name)))

  def toNegative(): Negative =
    Negative(name, array)

  def asCount(): Count =
    this.asInstanceOf[Count]

  def atLeast(): Int =
    this.asCount().least

  def asNegative(): Negative =
    this.asInstanceOf[Negative]

  def negate(): Predicate =
    if isNegative() then Predicate(name, array)
    else Negative(name, array)

  def toGeneric(names: Array[String]): Predicate =
    Predicate(name, names.take(getArity()).map(item => Variable(item)))

  def isDefinite() = array.forall(a => a.isSymbol())

  def isNegative() = false

  def isCount() = false

  def isMath() = false

  override def isList() = array.head.isList()

  override def isPredicate() = true

  override def isVariable() = false

  def length() = array.length

  override def contains(item: Variable): Boolean =
    array.contains(item)


  def identifier(): Int =
    name.hashCode * 7 + length()

  def combinations(elements: Array[String], length: Int): Array[Array[String]] =
    if (length == 1) elements.map(Array(_))
    else for {
      x <- elements
      xs <- combinations(elements, length - 1)
    } yield x +: xs


  def candidates(original: Array[String], names: Array[String]): Array[Predicate] =
    val crr = names ++ original
    combinations(crr, array.length)
      .filter(names => names.exists(name => original.contains(name)))
      .flatMap(items => {
        val variables = items.map(item => Variable(item))
        Array(Predicate(name, variables), Negative(name, variables))
      })

  override def hashCode(): Int =
    array.foldRight(name.hashCode) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Predicate] then
      val p = obj.asInstanceOf[Predicate]
      p.identifier() == identifier() &&
        p.array.zip(array).forall { case (a, b) => a.equals(b) }
    else
      false

  override def toString: String =
    name + "(" + array.map(_.toString).mkString(",") + ")"

  override def copy(): Variable =
    val copyArray = array.map(_.copy())
    Predicate(name, copyArray)

  def copy(newArray: Array[Variable]): Variable =
    Predicate(name, newArray)

class Negative(name: String, array: Array[Variable]) extends Predicate(name, array):

  def this(name: String, var1: Variable) = this(name, Array(var1))

  def this(name: String, var1: Variable, var2: Variable) = this(name, Array(var1, var2))

  def this(name: String, var1: Variable, var2: Variable, var3: Variable) = this(name, Array(var1, var2, var3))


  override def isNegative(): Boolean = true

  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "~" + super.toString()

  override def copy(): Variable =
    Negative(name, array.map(_.copy()))

  override def copy(newArray: Array[Variable]): Variable =
    Negative(name, newArray)

  override def substitution(substitution: Substitution): Predicate =
    val crrName = Variable(name)
    val newName = crrName.substitution(substitution).getName()
    val newArray = array.map(variable => variable.substitution(substitution))
    Negative(newName, newArray)

class Head(val head: Variable, val list: VariableList) extends Predicate("head", Array(head, list)):
  override def isDefinite(): Boolean =  true
  override def isList(): Boolean = list.nonEmpty()

  override def getValue(): Variable = {
    list.getHead()
  }
  override def copy(): Variable =
    Head(head.copy(), list.copy().asVariableList())

  override def substitution(substitution: Substitution): Variable =
    val newHead = head.substitution(substitution)
    val newList = list.substitution(substitution).asVariableList()
    Head(newHead, newList).asVariable()

class Tail(val tail: variables.VariableList, val list: variables.VariableList) extends Predicate("tail", Array(tail.asVariable(), list.asVariable())):
  override def isDefinite(): Boolean = true

  override def isList(): Boolean = list.nonEmpty()

  override def getValue(): Variable = {
    list.getTail()
  }
  override def copy(): Variable =
    Tail(tail.copy().asVariableList(), list.copy().asVariableList())

  override def substitution(substitution: Substitution): Predicate =
    val newTail = tail.substitution(substitution).asVariableList()
    val newList = list.substitution(substitution).asVariableList()
    Tail(newTail, newList)

class Count(name: String, array: Array[Variable], var least: Int) extends Predicate(name, array):

  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "Count(" + super.toString() + "," + least + ")"

  override def copy(): Count =
    Count(name, array.map(_.copy()), least)

  override def isCount(): Boolean = true

class Divide(array: Array[Variable]) extends Predicate("divide", array):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val headNumber = array.head.asNumber().getNumber()
    val lastNumber = array.last.asNumber().getNumber()
    variables.Num(name, headNumber / lastNumber)

  override def toString: String = "Divide(" + array.mkString(",") + ")"



class Mod(item: Variable, modBy: Variable) extends Predicate("mod", Array[Variable](item, modBy)):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val value1 = item.getValue().asNumber().getNumber()
    val value2 = modBy.getValue().asNumber().getNumber()
    variables.Num(name, math.floorMod(value1.toInt, value2.toInt))

  override def toString: String = "Mod(" + array.head.toString + ", " + array.last.toString + ")"

  override def copy(): Variable =
    Mod(item.copy().asNumber(), modBy.copy().asNumber())

  override def substitution(substitution: Substitution): Variable =
    val e1new = item.substitution(substitution)
    val e2new = modBy.substitution(substitution)
    Mod(e1new, e2new)


class Equal(e1: Variable, e2: Variable) extends Predicate("equal", Array[Variable](e1, e2)):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = {
    e1.getValue() == e2.getValue()
  }

  override def getValue(): Variable =
    val result = e1.getValue() == e2.getValue()
    new variables.Sym(name, result.toString)

  override def toString: String = "Equal(" + array.head.toString + ", " + array.last.toString + ")"

  override def copy(): Variable =
    Equal(e1.copy(), e2.copy())

  override def substitution(substitution: Substitution): Variable =
    val e1new = e1.substitution(substitution)
    val e2new = e2.substitution(substitution)
    Equal(e1new, e2new)


class Subtract(array: Array[Variable]) extends Predicate("subtract", array):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true


  override def getValue(): Variable =
    val headNumber = array.head.getValue().asInstanceOf[Double]
    val lastNumber = array.last.getValue().asInstanceOf[Double]
    variables.Num(name, headNumber - lastNumber)

  override def toString: String = "Subtract(" + array.head.toString + "," + array.last.toString + ")"


class Log(name: String, array: Array[Variable]) extends Predicate(name, array):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true
  override def getValue(): Variable =
    val item = math.log(array.head.getValue().asInstanceOf[Double])
    variables.Num(name, item)

  override def toString: String = "Log(" + array.head.toString + ")"

class Average(array: Array[Variable]) extends Predicate("average", array):
  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val items = array.filter(_.isNumberList()).map(_.asNumberList().average().getNumber())
    NumList(name, items)

  override def toString: String = "Average(" + array.mkString(",") + ")"

class Sum(array: Array[Variable]) extends Predicate("sum", array):

  def this(item: Variable) = this(Array(item))

  def this(item1: Variable, item2: Variable) = this(Array(item1, item2))

  def this(item1: Variable, item2: Variable, item3: Variable) = this(Array(item1, item2, item3))

  override def isMath(): Boolean = true
  override def isDefinite(): Boolean = true

  override def getValue(): Variable =
    val items = array.filter(_.isNumberList()).map(_.asNumberList().sum().getNumber())
    NumList(name, items)

  override def toString: String = "Sum(" + array.mkString(",") + ")"


