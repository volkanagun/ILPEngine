package ilp.dataset

import ilp.dataset.Dataset.main

import java.io.PrintWriter
import scala.io.Source

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

  def constructDunnhumby(ilpFilename:String, size:Int = 10000):Unit =
    val transactions = construct(dunnhumby + "transaction_data.csv", "transaction", Array(1, 3, 6)).take(size)
    val products = construct(dunnhumby + "product.csv", "product", Array(0, 1, 2)).take(size)
    val causal = construct(dunnhumby + "causal_data.csv", "causal", Array(0, 1, 4)).take(size)

    val data = transactions ++ products ++ causal
    new PrintWriter(ilpFilename){
      data.foreach(line=> println(line))
    }.close()


object Dataset extends Dataset:
  def main(args: Array[String]): Unit = {
    constructDunnhumby("bk.pl", 100000)
  }

