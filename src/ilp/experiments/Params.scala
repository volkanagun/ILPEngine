package ilp.experiments

import ilp.data.{Database, Engine, EngineMIL}

class Params:
  
  var experimentName = "kinship-pi"
  var engineType = "MIL"
  
  def getEngine(database:Database):Engine =
    if engineType == "MIL" then EngineMIL(database)
    else Engine(database)
