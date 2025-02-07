package ilp.data

class Predicate(name:String, var array:Array[Symbol|Collection|Variable]) extends Variable(name):
  var collection : Collection = null

  def setCollection(collection: Collection):this.type =
    this.collection = collection
    this

  def getArray():Array[Symbol|Collection|Variable] =
    this.array

  def isDefinite() = array.forall(a=> a.isSymbol()||a.isVariable())

  def isNegative() = false

  override def isPredicate() = true
  override def isVariable() = false
  def length() = array.length

  override def contains(item:Variable):Boolean =
    array.contains(item)

  def apply(items:Array[Symbol|Collection]):Predicate=
    Predicate(name, items.map(_.asInstanceOf[Variable]))

  def identifier():Int =
    name.hashCode * 7 + length()

  override def hashCode(): Int =
    array.foldRight(name.hashCode){case(a, m)=> a.hashCode() + 7 * m}

  override def equals(obj: Any): Boolean =
    if obj.isInstanceOf[Predicate] then
      val p = obj.asInstanceOf[Predicate]
      p.identifier() == identifier() &&
        p.array.forall(x => array.contains(x))
    else
      false

  override def toString: String =
    name + "(" + array.map(_.toString).mkString(",") + ")"

  override def copy(): Variable =
    val copyArray = array.map(_.copy())
    Predicate(name, copyArray)


class Negative(name: String, array:Array[Variable]) extends Predicate(name, array):

  override def apply(items: Array[Symbol | Collection]): Predicate =
    val superPredicate = apply(items)
    Negative(superPredicate.name, superPredicate.array)


  override def isNegative(): Boolean = true
  override def hashCode(): Int =
    super.hashCode()

  override def toString: String = "~" + super.toString()

  override def copy(): Variable =
    Negative(name, array.map(_.copy()))


