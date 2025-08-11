package ilp.data.program

import ilp.data.predicates.Predicate


final class Answer(var main: Substitution, var substitutions: Set[Substitution] = Set()) extends Serializable:

  def this(main: Substitution, content: Substitution) = this(main, Set(content))

  override inline def toString: String = {
    substitutions.mkString("|")
  }

  def execute(head: Predicate): Set[Predicate] =
    val newPredicates = substitutions.map(sub => {
      val newArray = head.getArray.map(variable => {
        if variable.isSymbol then variable
        else variable.substitution(sub)
      })
      Predicate(head.getName, newArray)
    })

    newPredicates


  def getCombinedSubstitutions: Set[Substitution] = substitutions.map(substitution => substitution.append(main))
    .toArray
    .toSet


