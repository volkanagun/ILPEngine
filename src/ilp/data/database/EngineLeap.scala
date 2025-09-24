package ilp.data.database

import ilp.data.optimization.Optimized
import ilp.data.predicates.Predicate
import ilp.data.program.Substitution

class EngineLeap(db:Database) extends Engine(db) {

  override def join(contextMap: Map[Int, Array[ExecutionContext]], programContext: ExecutionContext, currentContext: ExecutionContext): Set[Substitution] = ???
  override def join(programs: Array[Optimized], substitution: Substitution): Set[Substitution] = ???
  override def join(programs: Array[Optimized], callPredicate: Predicate): Set[Substitution] = ???

  def execute(optimized: Optimized, substitution: Substitution):Substitution =
    val context = ExecutionContext(optimized, substitution)
    null

}
