package ilp.data

class Operation(val function: Variable, var query: Array[Predicate], var items: Array[Variable]):

  override def hashCode(): Int =
    var r = function.hashCode()
    r = r * 7 + query.hashCode()
    items.foldRight(r) { case (a, m) => a.hashCode() + m * 7 }

  override def equals(obj: Any): Boolean =
    val other = obj.asInstanceOf[Operation]
    other.hashCode() == hashCode()

  def copy(): Operation =
    val definitionClone = function.copy()
    val queryClone = query.map(_.copy().toPredicate())
    val itemsClone = items.map(_.copy())
    Operation(definitionClone, queryClone, itemsClone)

  override def toString: String =
    function.toString + " :: " + query.map(_.toString).mkString(" && ") + " ==> " + items.mkString(" && ")

  def execute(instance: Predicate): Option[Operation] =
    val call = Substitution().of(function, instance)
    if call.isDefined then
      val main = call.get
      val newFunction = main.of(function)
      val newQuery = query.map(variable => main.of(variable).toPredicate())
      Some(Operation(newFunction, newQuery, items))
    else
      Some(Operation(function, query, items))



