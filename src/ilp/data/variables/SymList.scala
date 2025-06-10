package ilp.data.variables

class SymList(name:String, var items:Array[String]) extends Sym(name, items.mkString(",")) :

  def this(name:String) = this(name, Array[String]())
  def this(name:String, var1:String) = this(name, Array(var1))
  def this(name:String, var1:String, var2:String) = this(name, Array(var1, var2))
  def this(name:String, var1:String, var2:String, var3:String) = this(name, Array(var1, var2, var3))

  override def isNumberList(): Boolean = true
  override def isSymbol(): Boolean = true
  override def getSize():Int = items.size
  override def isEmpty(): Boolean = items.isEmpty
  override def copy(): Variable = SymList(name, items)


  def nonEmpty() : Boolean = items.nonEmpty
  def getHead(): Sym = Sym("X", items.head)
  def append(sym:Sym): SymList = SymList(name, items:+sym.value)
  def prepend(sym:Sym): SymList = SymList(name, sym.value +: items)
  def reverse(): SymList = SymList(name, items.reverse)

  //def getHead(name:String): Num = Num(name, value.head)
  def getTail(): SymList = SymList(name, items.tail)
  def getTail(name:String): SymList = SymList(name, items.tail)

  override def hashCode(): Int = name.hashCode()
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Variable] && obj.asInstanceOf[Variable].name == name
  override def toString: String = items.mkString("[",",","]")
