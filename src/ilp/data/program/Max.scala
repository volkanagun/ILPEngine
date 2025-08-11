package ilp.data.program

class Max(var name:String, size:Double) extends Serializable{

  def isVariables: Boolean = name.startsWith("max_vars")
  def isClause: Boolean = name.startsWith("max_clauses")
  def isBody: Boolean = name.startsWith("max_body")
}
