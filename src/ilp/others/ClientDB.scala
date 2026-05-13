package ilp.others

import ilp.data.database.Database

abstract class ClientDB(val db:Database, val name:String) {
  def createDB():ClientDB
  def queryWebkb():Double
  def queryZendo():Double
  def queryCentipente():Double
  def queryPTC():Double
  def queryPTE():Double
  def queryYeast():Double
  def clearDB():ClientDB = {this}

  def measureTime[T](block: => T): Double = {

    val time = {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      val elapsedTime = (end - start) / 1e6
      elapsedTime
    }

    time
  }
}
