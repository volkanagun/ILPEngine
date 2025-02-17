package ilp.data



class Predicate(crr_name: String, var array: Array[Symbol | Collection | Variable]) extends Variable(crr_name):
  var collection: Collection = null
  
  def setCollection(collection: Collection): this.type =
    this.collection = collection
    this

  def getArray(): Array[Symbol | Collection | Variable] =
    this.array
    
  def getArity():Int =
    this.array.length  

  def getVariables():Array[Variable] =
    array.map(_.asVariable())

  def getLiterals():Array[String] =
    array.map(item => item.name) ++
      array.filter(_.isSymbol()).map(_.asInstanceOf[Symbol].value)

  def bindTo(predicate: Predicate):Predicate =
    val otherVars = predicate.array.filter(_.isVariable())
    Predicate(name, otherVars)
    
  def bindTo(elements: Array[Variable]):Predicate =
    Predicate(name, elements)

  def getReplace(names:Array[String]):Substitution =
    val vars = array.map(item => Variable(item.name))
    val reps = names.map(name=> Variable(name))
    Substitution(vars, reps)

  override def getComplexity():Double =
    val symbolComplexity = array.foldRight(0.0){case(s, m)=> s.getComplexity() + m}
    if isNegative() then 2d * symbolComplexity
    else symbolComplexity

  override def substitution(substitution: Substitution):Predicate =
    val newName = substitution.of(Variable(name)).getName()
    val newArray = array.map(variable=> variable.substitution(substitution))
    Predicate(newName, newArray)

  def toPredicate(newName:String):Predicate =
    Predicate(newName, array.map(_.copy()))


  def toGeneric():Predicate =
    Predicate(name, array.map(item => Variable(item.name)))


  def toNegative():Negative =
    Negative(name, array)

  def toGeneric(names:Array[String]):Predicate =
    Predicate(name, names.take(getArity()).map(item => Variable(item)))

  def isDefinite() = array.forall(a => a.isSymbol())

  def isNegative() = false

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


  def candidates(original:Array[String], names: Array[String]): Array[Predicate] =
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


class Negative(name: String, array: Array[Variable]) extends Predicate(name, array):

  override def isNegative(): Boolean = true

  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "~" + super.toString()

  override def copy(): Variable =
    Negative(name, array.map(_.copy()))


object Predicate extends Predicate("p", Array(Variable("X"))):

  def str(item:String):Variable =
    if item.contains("(") then
      val po = item.indexOf("(")
      val pc = item.lastIndexOf(")")
      val name = item.substring(0, po)
      val inputs = item.substring(po + 1, pc)
      val items = inputs.split("(\\,\\s?)").map(element=>{
        str(element.trim)
      })
      new Predicate(name, items)
    else if item.head.isLower then
      new Symbol("X", item)
    else
      new Variable(item)

  def main(args: Array[String]): Unit = {

    println(Predicate.str("parent(X, func(e, Y))"))

  }
