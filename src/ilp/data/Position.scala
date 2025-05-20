package ilp.data

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class Position(val predicate:Predicate, val pindex:Int, val index:Int) {
  override def hashCode(): Int = (predicate.identifier().hashCode() * 7 + pindex) * 7 + index

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Position]
    other.predicate.identifier() == predicate.identifier() && other.pindex == pindex && other.index == index
  }

  def equalsByName(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Position]
    other.getVariable().equals(getVariable())
  }

  override def toString = predicate.name + "/"+ pindex + "_" + index + s"_${getName()}"
/*

  def getPositions():Array[Position] =
    predicate.getPositions(pindex)
*/

  def getValueIdentifier():Int =
    predicate.identifier() * 7 + index

  def getVariableIdentifier():Int =
    getVariable().hashCode()

  def getVariable():Variable =
    predicate.getVariable(index)

  def getName():String =
    predicate.getVariable(index).getName()

  def getPredicate():Predicate =
    predicate

  def getIdentifier():Int =
    predicate.identifier()

  def getPredicateIdentifier():Int =
    predicate.identifier(pindex)



  def getIndex():Int =
    index

  /*
  def getBindWith(position: Position):Predicate =
    getPredicate().substitution(Substitution(getVariable(), position.getVariable()))
      .asPredicate()

  def getBindWith(position: Set[Position]):Set[Predicate] =
    getPredicate().getArray().combinations(position.size).map(variables=>{
      val elements = position.zip(variables).map{case(p, v)=> (p.getVariable(), v)}.toArray
      getPredicate().substitution(Substitution(elements)).asPredicate()
    }).toSet
  */
}
