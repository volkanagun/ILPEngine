package ilp.data

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
    other.getVariable().equals(getVariable())
  }

  override inline def toString = predicate.name + "/"+ pindex + "_" + index + s"_${getName()}"


/*
  inline def getValueIdentifier():Int =
    predicate.identifier() * 7 + index*/
/*
  def getVariableIdentifier():Int =
    getVariable().hashCode()*/

  inline def getVariable():Variable =
    predicate.getVariable(index)

  inline def getName():String =
    predicate.getVariable(index).getName()

/*  def getPredicate():Predicate =
    predicate*/

  inline def getIdentifier():Int =
    predicate.identifier()

  inline def getPredicateIdentifier():Int =
    predicate.identifier(pindex)

  inline def getIndex():Int =
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
