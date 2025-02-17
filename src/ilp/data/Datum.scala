package ilp.data

class Variable(var name: String):

  override def hashCode(): Int = name.hashCode()

  override def equals(obj: Any): Boolean = {
    name.equals(obj.asInstanceOf[Variable].name)
  }

  override def toString: String = name.toUpperCase

  def getName():String = name
  
  def setName(name:String):this.type =
    this.name = name
    this
  
  def getComplexity():Double = 1.0

  def substitution(substitution: Substitution): Variable =
    val newVariable = substitution.of(this)
    newVariable
  
  def toSymbol(value: String): Symbol =
    new Symbol(name, value)

  def asPredicate(): Predicate =
    this.asInstanceOf[Predicate]

  def asVariable(): Variable =
    this.asInstanceOf[Variable]

  
  def toVariable(): Variable =
    Variable(name)

  def candidates(names:Array[String]): Array[Variable] =
    names.map(name=> Variable(name)) 

  def contains(item: Variable) = false

  def isSymbol() = false

  def isPredicate() = false

  def isVariable() = true

  def copy(): Variable = new Variable(name)

  def of(name: String) = new Variable(name)

class Symbol(name: String, var value: String) extends Variable(name):

  override def getComplexity(): Double = 0

  override def isSymbol() = true

  override def isPredicate() = false

  override def isVariable() = false

  override def copy(): Variable = new Symbol(name, value)

  override def hashCode(): Int = value.hashCode

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Symbol] then
      val other = obj.asInstanceOf[Symbol]
      other.value.equals(value)
    else
      name.equals(obj.asInstanceOf[Variable].name)

  override def toString: String = value.toLowerCase(Settings.locale)

class Collection(name: String, var values: Set[Symbol]) extends Symbol(name, name):

  override def hashCode(): Int = name.hashCode()

  override def equals(obj: Any): Boolean = name.equals(obj.asInstanceOf[Variable].name)

  override def isSymbol() = true

  override def isPredicate() = false

  override def isVariable() = false

  override def copy(): Variable =
    Collection(name, values)


class Update(var head: Array[Predicate], var body: Array[Predicate]):

  def queryHash(): Int =
    head.foldRight(0) { case (a, m) => a.hashCode() + 7 * m }


  override def hashCode(): Int =
    val hash = head.foldRight(0) { case (a, m) => a.hashCode() + 7 * m }
    body.foldRight(hash) { case (a, m) => a.hashCode() + 7 * m }

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Update] then
      obj.asInstanceOf[Update].hashCode() == hashCode()
    else
      false

  override def toString: String =
    head.mkString(" & ") + " ==> " + body.mkString(" & ")

  def copy(): Update =
    val headCopy = head.map(_.copy().asPredicate())
    val bodyCopy = body.map(_.copy().asPredicate())
    Update(headCopy, bodyCopy)