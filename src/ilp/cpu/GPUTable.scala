package ilp.cpu

import ilp.data.variables.Variable

class GPUTable(var id: Int, var pid: Int, var attributes: Array[Variable], var data: Array[Array[Int]], var rows:Array[Int]):
  val rowSize = data.length
  var gpuData = data.flatten
  var colSize = attributes.length

  def this(id:Int, pid:Int, attributes:Array[Variable], data:Array[Array[Int]]) = this(id, pid, attributes, data, Array.fill[Int](data.length)(1))

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


  def getNew(rows: Array[Int]): GPUTable =
    GPUTable(id, pid, attributes, data, rows)
