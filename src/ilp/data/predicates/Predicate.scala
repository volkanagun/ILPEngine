package ilp.data.predicates

import ilp.data.variables.{Collection, NumList, Variable, VariableList}
import ilp.data.*



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


  def execute(): Option[Substitution] = None


  def getVariable(index: Int): Variable =
    array(index)

  def getSymbol(index: Int): variables.Sym =
    array(index).asSymbol()

  def getVariables(): Array[Variable] =
    array.map(_.asVariable())

  def getSymbols(): Array[variables.Sym] =
    array.map(_.asSymbol())

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
  def isExecutable() = false

  //def isExecutable() = isMath() && isDefinite()
  //override def isList() = array.head.isList()

  override def isPredicate() = true

  override def isVariable() = false

  override def isEmpty(): Boolean = array.forall(_.isEmpty())

  def length() = array.length

  override def contains(item: Variable): Boolean =
    array.contains(item)


  def identifier(): Int =
    name.hashCode * 7 + length()

  def identifier(position:Int): Int =
    (position * 7 + name.hashCode) * 7 + length()

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

class Count(name: String, array: Array[Variable], var least: Int) extends Predicate(name, array):

  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "Count(" + super.toString() + "," + least + ")"

  override def copy(): Count =
    Count(name, array.map(_.copy()), least)

  override def isCount(): Boolean = true




