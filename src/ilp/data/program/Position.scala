package ilp.data.program

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

final class Position(val predicate:Predicate, val pindex:Int, val index:Int) extends Serializable{
  override inline def hashCode(): Int = (predicate.identifier().hashCode() * 7 + pindex) * 7 + index

  override inline def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Position]
    other.predicate.identifier() == predicate.identifier() && other.pindex == pindex && other.index == index
  }

  inline def equalsByName(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Position]
    other.getVariable.equals(getVariable)
  }

  override inline def toString: String = predicate.name + "/"+ pindex + "_" + index + s"_${getName}"

  inline def getVariable:Variable =
    predicate.getVariable(index)

  inline def getName:String =
    predicate.getVariable(index).getName

  inline def getIdentifier:Int =
    predicate.identifier()

  inline def getPredicateIdentifier:Int =
    predicate.identifier(pindex)

  inline def getIndex:Int =
    index

}
