package ilp.cpu

import ilp.data.variables.Variable

class CPUQueryPlan(var query:GPUQuery):

  def optimizeByCount():GPUQuery =
    println("Query optimization has started...")
    var tables = query.tables
    val attributes = query.getAttributes().sortBy(variable=>{
      tables.filter(table=> table.contains(variable))
        .map(table => table.getActiveSet(variable).size).max
    }).reverse

    tables = tables.sortBy(table=>{
      attributes.filter(variable=> table.contains(variable))
        .map(variable=> table.getActiveSet(variable).size).max
    }).reverse


    val newQuery = GPUQuery(tables).setAttributes(attributes).init()
    println("Query optimization finished...")
    newQuery
    //query

