package ilp.gpu

import ilp.data.{Position, Query}
import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

import scala.util.control.Breaks

class JoinResult(var values: Map[Int, Array[Int]], var joins: Map[String, JoinRow]):

  var result:Array[Map[Int, JoinMerge]] = null

  def this() = this(Map(), Map())

  def add(joinResult: JoinResult): this.type =
    values = values ++ joinResult.values
    joins = joins ++ joinResult.joins
    this

  def merge(map1:Map[Int, JoinMerge], map2:Map[Int, JoinMerge]) : Map[Int, JoinMerge] =
    var mergeMap = map1
    map1.filter{case(p1,_)=>{map2.contains(p1)}}.map {case(p1, rowMap1) => {
      val rowMap2 = map2(p1)
      p1-> rowMap1.merge(rowMap2)
    }}.foreach{case(id, rowMap)=>{
      mergeMap = mergeMap.updated(id, rowMap)
    }}
    mergeMap

  def merge():this.type =
    result = joins.map{case(_, joinRow) => joinRow.map()}.toArray
    for(i<-0 until result.size - 1){
      val crrMap = result(i)
      for (j<-i+1 until result.size){
        result(j) = merge(result(j), crrMap)
      }
    }
    this