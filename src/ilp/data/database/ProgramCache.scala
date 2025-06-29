package ilp.data.database

import ilp.data.Substitution

import scala.collection.concurrent.TrieMap as ConcurrentMap
class ProgramCache {
  var cache = ConcurrentMap[Int, Set[Substitution]]()

  def contains(id:Int):Boolean =
    cache.contains(id)

  def update(id:Int, set:Set[Substitution]): Set[Substitution] = {
    cache.update(id, set)
    set
  }

  def get(id:Int):Set[Substitution]=
    cache(id)
}
