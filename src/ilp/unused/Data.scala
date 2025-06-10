import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class DataSymbol(var ruleHead:Predicate, var map: Map[Int, Set[Predicate]], var relations: Array[Predicate], var attributes: Array[Variable]) {
  def contains(id:Int) = map.contains(id)
  def body_id():Int =
    relations.map(p=> p.identifier()).foldRight[Int](3){case(a, m)=> a + 7 * m}

  def setMap(map:Map[Int, Set[Predicate]]):this.type = {
    this.map = map
    this
  }

  def callMap(filteredMap:Map[Int, Set[Predicate]]):DataSymbol=
    DataSymbol(ruleHead, filteredMap, relations, attributes)

  def callAttribute(index:Int, attribute:Variable):Variable=
    val crr = ruleHead.getVariable(index)
    val newAttribute = attribute.copy().setName(crr.getName())
    newAttribute

  def call(newAttribute:Variable):DataSymbol=
    val newAttributes = attributes.map(attribute=>{
      if attribute.getName() == newAttribute.getName() then newAttribute
      else attribute
    })

    DataSymbol(ruleHead, map, relations, newAttributes)
}

case class DataBitset()
