package ilp.others

import ilp.data.database.Database

abstract class ClientDB(val db:Database, val name:String) {
  def createDB():ClientDB
  def queryWebkb():Unit
  def queryZendo():Unit
  def queryCentipente():Unit
  def queryPTC():Unit
  def queryPTE():Unit
  def queryYeast():Unit
  def clearDB():ClientDB = {this}
}
