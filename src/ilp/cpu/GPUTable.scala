package ilp.cpu

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

class GPUTable(var predicate: Predicate, var name:String, var id: Int, var pid: Int, var attributes: Array[Variable], var data: Array[Array[Int]], var rows:Array[Int]):

  val rowSize = data.length
  var gpuData = data.flatten
  var colSize = attributes.length


  def this(predicate: Predicate, name:String, id:Int, pid:Int, attributes:Array[Variable], data:Array[Array[Int]]) = this(predicate,
    name, id, pid, attributes, data, Array.fill[Int](data.length)(1))

  def setGPUData(gpuData: Array[Int]): this.type =
    this.gpuData = gpuData
    this



  def contains(variable: Variable):Boolean =
    attributes.contains(variable)

  def getIndex(variable: Variable):Int =
    attributes.indexOf(variable)

  def getCPUData(): Array[Array[Int]] =
    gpuData.sliding(colSize).toArray.zipWithIndex
      .filter{case(row, index)=> rows(index)==1}
      .map(_._1)

  def getActiveSet(newRows:Array[Int], attribute: Variable): Set[Int] =
    val index = attributes.indexOf(attribute)
    data.zipWithIndex
      .filter { case (row, rowIndex) => newRows(rowIndex)==1 }
      .map{case(row, _)=> row(index)}.toSet

  def getActiveSet(attribute: Variable): Set[Int] =
    val index = attributes.indexOf(attribute)
    data.zipWithIndex
      .filter { case (row, rowIndex) => rows(rowIndex)==1 }
      .map{case(row, _)=> row(index)}.toSet

  def getActiveSize(attribute: Variable): Int =
    val index = attributes.indexOf(attribute)
    data.zipWithIndex
      .filter { case (row, rowIndex) => rows(rowIndex)==1 }
      .map{case(row, _)=> row(index)}.toSet.size

  def getActiveSize(newRows:Array[Int], attribute: Variable): Int =
    val index = attributes.indexOf(attribute)
    data.zipWithIndex
      .filter { case (row, rowIndex) => newRows(rowIndex)==1 }
      .map{case(row, _)=> row(index)}.toSet.size

  def getRelationSize(attribute: Variable): Int =
    val index = attributes.indexOf(attribute)
    data.zipWithIndex
      .filter { case (row, rowIndex) => rows(rowIndex)==1 }
      .map {case(row, _)=> row.zipWithIndex.filter{case(value, indice) => indice!=index}
        .map(_._1).mkString("_")}.toSet.size

  def getInstanceSize(newRows:Array[Int], attribute: Variable, value:Int): Int =
    val index = attributes.indexOf(attribute)
    data.zipWithIndex
      .filter { case (row, rowIndex) => newRows(rowIndex)==1 && row(index) == value}
      .size

  def getDependencyScore(source: Variable, destination:Variable): Double =
    val sourceIndex = attributes.indexOf(source)
    val destinationIndex = attributes.indexOf(destination)

    if sourceIndex >= 0 && destinationIndex >= 0 then
      val sourceSize = data.map(row => row(sourceIndex)).toSet.size
      val destionationSize = data.map(row => row(destinationIndex)).toSet.size
      /*sourceSize.toDouble / */destionationSize
    else
      1d

  override def toString = predicate.toString

  def getNew(rows: Array[Int]): GPUTable =
    GPUTable(predicate, name,id, pid, attributes, data, rows)
