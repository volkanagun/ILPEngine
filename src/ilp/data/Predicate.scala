package ilp.data

class Predicate(name: String, var array: Array[Symbol | Collection | Variable]) extends Variable(name):
  var collection: Collection = null

  def setCollection(collection: Collection): this.type =
    this.collection = collection
    this

  def getArray(): Array[Symbol | Collection | Variable] =
    this.array
  
  def getVariables():Array[Variable] =
    array.map(_.toVariable())

  def getLiterals():Array[String] =
    array.map(item => item.name) ++ 
      array.filter(_.isSymbol()).map(_.asInstanceOf[Symbol].value)

  def toGeneric():Predicate =
    Predicate(name, array.map(item => Variable(item.name)))
  
  def isDefinite() = array.forall(a => a.isSymbol())

  def isNegative() = false

  override def isPredicate() = true

  override def isVariable() = false

  def length() = array.length

  override def contains(item: Variable): Boolean =
    array.contains(item)

  /*def apply(items: Array[Symbol | Collection]): Predicate =
    Predicate(name, items)*/

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

  /*  override def apply(items: Array[Symbol | Collection]): Predicate =
      val superPredicate = apply(items)
      Negative(superPredicate.name, superPredicate.array)*/


  override def isNegative(): Boolean = true

  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "~" + super.toString()

  override def copy(): Variable =
    Negative(name, array.map(_.copy()))


object Predicate extends Predicate("p", Array(Variable("X"))):
  def main(args: Array[String]): Unit = {
    val names = Array("a", "b", "c")
    combinations(names, 2).foreach(seq => println(seq.mkString(",")))
  }
