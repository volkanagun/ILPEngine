package ilp.data.variables

import ilp.data.Substitution
import ilp.data.predicates.Predicate


class Answer(var main: Substitution, var substitutions: Set[Substitution] = Set()):

  def this(main: Substitution, content: Substitution) = this(main, Set(content))

  override def toString: String = {
    substitutions.mkString("|")
  }

  def execute(head: Predicate): Set[Predicate] =
    val newPredicates = substitutions.map(sub => {
      val newArray = head.getArray().map(variable => {
        if variable.isSymbol() then variable
        else variable.substitution(sub)
      })
      Predicate(head.getName(), newArray)
    })

    newPredicates


  def getCombinedSubstituions(): Set[Substitution] = substitutions.map(substitution => substitution.append(main))
    .toArray
    .toSet


/*
  def isEmpty(): Boolean =
    substitutions.isEmpty

  def setMain(main: Substitution): this.type =
    this.main = main
    this

  def setSubstitutions(substitutions: Set[Substitution]): this.type =
    this.substitutions = substitutions
    this

  def getSubstitutions(): Set[Substitution] = substitutions
*/
