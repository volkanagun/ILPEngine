package ilp.others

import ilp.data.database.Database
import ilp.experiments.Performance

object Comparisons {

  val names = Array( //"/media/wolf/Corsair/java-projects/ILPEngine/examples/noisy-zendo2-10",
    "/media/wolf/Corsair/java-projects/ILPEngine/examples/ptc" //,
    //"/media/wolf/Corsair/java-projects/ILPEngine/examples/pte",
    //"/media/wolf/Corsair/java-projects/ILPEngine/examples/webkb",
    //"/media/wolf/Corsair/java-projects/ILPEngine/examples/yeast",
    /*"/media/wolf/Corsair/java-projects/ILPEngine/examples/iggp-gt_centipede-goal"*/)

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


  def testDB(): Unit = {

    val clients = names.foreach(name => {
        val db = Performance.loadDatabase(name)
        val array = Array(new Jena(db).createDB())
        array.foreach(client=>{
          if (name.contains("centipede")) {
            val tcentipede = client.queryCentipente()
            println(s"Time for ${client.db.name} at ${client.name} is ${tcentipede.toString}")
          }
          else if (name.contains("zendo")) {
            val tzendo = client.queryZendo()
            println(s"Time for ${client.db.name} at ${client.name} is ${tzendo.toString}")
          }
          else if (name.contains("webkb")) {
            val twebkb = client.queryWebkb()
            println(s"Time for ${client.db.name} at ${client.name} is ${twebkb.toString}")
          }
          else if (name.contains("ptc")) {
            val tptc = client.queryPTC()
            println(s"Time for ${client.db.name} at ${client.name} is ${tptc.toString}")
          }
          else if (name.contains("pte")) {
            val tptc = client.queryPTE()
            println(s"Time for ${client.db.name} at ${client.name} is ${tptc.toString}")
          }
          else if (name.contains("yeast")) {
            val tptc = client.queryYeast()
            println(s"Time for ${client.db.name} at ${client.name} is ${tptc.toString}")
          }
        })
      })
  }

  def main(args: Array[String]): Unit = {

    testDB()
  }

}
