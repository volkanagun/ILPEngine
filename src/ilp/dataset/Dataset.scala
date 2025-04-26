package ilp.dataset

import ilp.dataset.Dataset.main

import java.io.PrintWriter
import scala.io.Source
import scala.util.Random

class Dataset():

  val dunnhumby = "resources/datasets/dunnhumby/"
  def readCSV(filename:String):Array[Array[String]] =
    Source.fromFile(filename).getLines().toArray.tail.map(line=>{
      line.split("\\,")
    })

  def construct(filename:String, predicate:String, array:Array[Int]):Array[String] =
    val csv = readCSV(filename)
    csv.map(line=> array.map(i=> line(i).toLowerCase.replaceAll("[\\s\\-\\_\\.\\/\\&]","").trim).filter(_.nonEmpty))
      .filter(items=> items.length == array.length).map(items=> items.mkString(predicate + "(",",",")."))

  def construct(predicate:String, size:Int, attrSize:Int, range:Int):Array[String] =
    Range(0, size).map(_=> {
      predicate +"(" + Range(0, attrSize).map(_=>{
        Random.nextInt(range)
      }).mkString(",") + ")."
    }).toArray



  def constructDunnhumby(ilpFilename:String, min:Int = 10000, max:Int = 20000):Unit =
    val t1 = min + Random().nextInt(max - min)
    val t2 = min + Random().nextInt(max - min)
    val t3 = min + Random().nextInt(max - min)

    val tt = construct(dunnhumby + "transaction_data.csv", "transaction", Array(1, 3, 6))
    val pp = construct(dunnhumby + "product.csv", "product", Array(0, 1, 2))
    val cc = construct(dunnhumby + "causal_data.csv", "causal", Array(0, 1, 4))

    val transactions = Random().shuffle(tt.toSeq).take(t1)
    val products = Random().shuffle(pp.toSeq).take(t2)
    val causal = Random().shuffle(cc.toSeq).take(t3)

    val data = transactions ++ products ++ causal
    new PrintWriter(ilpFilename){
      data.foreach(line=> println(line))
    }.close()



  def constructRandom(ilpFilename:String, tableSize:Int, valueRange:Int, attrMin:Int = 2, attrMax:Int = 6):Unit = {
    val min = 300
    val max = 500
    val data = Range(0, tableSize).flatMap(tableIndex=>{
      val attributeSize = attrMin + Random.nextInt(attrMax-attrMin)
      val name = "predicate"+tableIndex
      val size = min + Random.nextInt(max-min)
      construct(name, size, attributeSize, valueRange)
    })

    new PrintWriter(ilpFilename){
      data.foreach(line=> println(line))
    }.close()
  }


object Dataset extends Dataset:
  def main(args: Array[String]): Unit = {
    constructRandom("bk.pl", 6, 5, 4, 5)
  }

