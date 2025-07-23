package ilp.data

class Max(var name:String, size:Double) extends Serializable{

  def isVariables() = name.startsWith("max_vars")
  def isClause() = name.startsWith("max_clauses")
  def isBody() = name.startsWith("max_body")
}
