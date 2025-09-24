package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.program.{Hypothesis, Parser, Rule}
import ilp.data.variables.Variable

import java.io.File
import scala.io.Source
import scala.collection.mutable.{Map=>HashMap}
class Bias extends Serializable{

  case class Function(name: String, variables: Array[Position]) {
    def identifier(): Int =
      name.hashCode * 7 + variables.length

    def contains(position: Position): Boolean =
      variables.contains(position)
  }

  case class Position(function: String, name: String, index: Int) {
    override def hashCode(): Int = function.hashCode() * 7 + index

    override def equals(obj: Any): Boolean = {
      val other = obj.asInstanceOf[Position]
      other.index == index && other.function.equals(function)
    }

    override def toString: String = function + "/"+index +"=" + name
  }

  var predicates: HashMap[String, Function] = HashMap[String, Function]()
  var map: HashMap[Position, Position] = HashMap[Position, Position]()


  //noinspection SourceNotClosed
  def build(filename: String): this.type = {
    if File(filename).exists() then
      val definitions = Source.fromFile(filename).getLines().filter(line => !line.startsWith("%") || line.trim.isEmpty)
        .filter(line => line.startsWith("type")||line.startsWith("constant"))
        .flatMap(line => Parser.parseDefinition(line))

      predicates = HashMap.from(definitions.map(function => {
        val positions = function.variables.zipWithIndex.map { case (name, index) =>
          Position(function.name, name, index)
        }
        function.name -> Function(function.name, positions)
      }).toMap)

      map = HashMap.from(predicates.toArray.flatMap { case (name, function) => function.variables.map(position => (position, position)) }
        .toMap)

    this
  }

  def getPositions(predicate: Predicate): Array[Position] =
    predicate.getVariables.zipWithIndex.map { case (variable, index) =>
      Position(predicate.getName, variable.getName, index)
    }

  def getRule(rule: Rule, map:Map[Position, Position] = map.toMap): Option[Function] = {
    var result = Map[String, String]()

    rule.getBody.flatMap(predicate => {
      getPositions(predicate)
    }).foreach { position => {
      val positionName = position.name
      if result.contains(positionName) && map.contains(position) then {
        val crrName = result(positionName)
        val crrCategory = map(position).name
        if crrName != crrCategory then
          return None
      }
      else if map.contains(position) then {
        val crrCategory = map(position).name
        result = result.updated(position.name, crrCategory)
      }
    }
    }

    val predicate = rule.getHead
    val finalResult = predicate
      .getVariables.zipWithIndex
      .map { case (variable, index) => //noinspection RedundantBlock
      {
        val position = if result.contains(variable.getName) then {
          val category = result(variable.getName)
          Position(predicate.getName, category, index)
        }
        else {
          Position(predicate.getName, variable.getName, index)
        }

        position
      }
      }

    Some(Function(predicate.getName, finalResult))
  }

  def getHypothesis(hypothesis: Hypothesis): Option[Map[Position, Position]] = {
    var crrMap = map.toMap
    if map.isEmpty then Some(map.toMap)
    else {
      hypothesis.getSorted.foreach(rule => {
        val functionOpt = getRule(rule, crrMap)
        if functionOpt.isEmpty then return None
        else {
          val function = functionOpt.get
          val newMap = function.variables.map(position => position -> position)
          crrMap = crrMap ++ newMap
        }
      })

      Some(crrMap)
    }
  }
}
