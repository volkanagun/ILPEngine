package ilp.data.database
import ilp.data.Substitution
import ilp.data.variables.Variable

import scala.collection.concurrent.TrieMap as ConcurrentMap

class ExecutionCache {

  var cache = ConcurrentMap[Int, Set[(ContextData, Set[Substitution])]]()

  def id(context: ContextData, predicateId:Int): Int = {
    val items = Array(predicateId, context.getRuleId(), context.getSubstitution().id())
    items.foldRight(1) { case (crr, main) => main * 7 + crr }
  }

  def get(crrId:Int): Option[Set[(ContextData, Set[Substitution])]] = {
    cache.get(crrId)
  }

  def update(crrId:Int, contextData: ContextData, set: Set[Substitution]): Unit = {
    if cache.contains(crrId) then {
      val newSet = cache(crrId) + ((contextData, set))
      cache.update(crrId, newSet)
    } else
      cache.update(crrId, Set((contextData, set)))
  }

  def update(crrId:Int, data:Set[(ContextData, Set[Substitution])]): Unit = {
    data.foreach{case(context, set)=>{
      update(crrId, context, set)
    }}
  }


}
