package ilp.others

import ilp.data.database.Database
import ilp.experiments.Performance

object Comparisons{

  val names = Array("/media/wolf/Corsair/java-projects/ILPEngine/examples/noisy-zendo2-10",
    "/media/wolf/Corsair/java-projects/ILPEngine/examples/ptc",
    "/media/wolf/Corsair/java-projects/ILPEngine/examples/pte",
    "/media/wolf/Corsair/java-projects/ILPEngine/examples/webkb",
    "/media/wolf/Corsair/java-projects/ILPEngine/examples/yeast",
    "/media/wolf/Corsair/java-projects/ILPEngine/examples/iggp-gt_centipede-goal")

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

  def initialize():Unit = {
     names.map(name=> Performance.loadDatabase(name))
       .foreach(db=> {
         //new Virtuoso(db).clearDB().createDB()
         //new Postgres(db).clearDB().createDB()
         //new Tentris(db).createDB()
         //new MilleniumDB(db).createDB()
         new Jena(db).createDB()
       })

  }

  def testDB(): Unit = {
    val clients = names.map(name=> Performance.loadDatabase(name))
      .map(db=> {
        //new Virtuoso(db)
        //new Postgres(db)
        //new MilleniumDB(db)
        new Jena(db)
      })
    for (client <-clients) {
      if (client.db.name.contains("centipede")) {
        val tcentipede = measureTime(client.queryCentipente())
        println(s"Time for ${client.db.name} at ${client.name} is ${tcentipede.toString}")
      }
      else if(client.db.name.contains("zendo")){
        val tzendo = measureTime(client.queryZendo())
        println(s"Time for ${client.db.name} at ${client.name} is ${tzendo.toString}")
      }
      else if(client.db.name.contains("webkb")){
        val twebkb = measureTime(client.queryWebkb())
        println(s"Time for ${client.db.name} at ${client.name} is ${twebkb.toString}")
      }
      else if(client.db.name.contains("ptc")){
        val tptc = measureTime(client.queryPTC())
        println(s"Time for ${client.db.name} at ${client.name} is ${tptc.toString}")
      }
      else if(client.db.name.contains("pte")){
        val tptc = measureTime(client.queryPTE())
        println(s"Time for ${client.db.name} at ${client.name} is ${tptc.toString}")
      }
      else if(client.db.name.contains("yeast")){
        val tptc = measureTime(client.queryPTE())
        println(s"Time for ${client.db.name} at ${client.name} is ${tptc.toString}")
      }
      else{
        0.0
      }
    }
  }

  def main(args: Array[String]): Unit = {
    initialize()
    testDB()
  }

}
