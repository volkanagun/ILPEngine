package ilp.data.database

import ilp.data.predicates.Predicate
import ilp.data.program.Substitution


import scala.collection.concurrent.TrieMap as ConcurrentMap

class ExecutionCache extends Serializable{

  var cache = ConcurrentMap[Int, Set[(ExecutionContext, Set[Substitution])]]()

  def id(context: ExecutionContext, predicate:Predicate): Int = {
    context.getExecutionId(predicate)
  }

  def contains(id:Int) : Boolean = cache.contains(id)

  def get(crrId: Int): Option[Set[(ExecutionContext, Set[Substitution])]] = {
    val result = cache.get(crrId)
    if result.isDefined then
      Some(result.get.map(pair=> (pair._1, pair._2)))
    else
      None
  }

  def getFlat(crrId: Int): Set[(ExecutionContext, Set[Substitution])] = {
    val result = cache(crrId)
    result.map(pair=> (pair._1, pair._2))
  }

  def update(crrId: Int, contextData: ExecutionContext, set: Set[Substitution]): Unit = {
    if cache.contains(crrId) then {
      val newSet = cache(crrId) + ((contextData, set))
      cache.update(crrId, newSet)
    } else {
      cache.update(crrId, Set((contextData, set)))
    }
  }

  def update(crrId: Int, data: Set[(ExecutionContext, Set[Substitution])]): Unit = {
    data.foreach { case (context, set) => {
      update(crrId, context, set)
    }
    }
  }


}
