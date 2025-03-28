package ilp.cpu

import ilp.data.variables.Variable

class GPUQuery(var tables: Array[GPUTable]):

  var gpuData: Array[Array[Int]] = null
  var gpuDataRows: Array[Array[Int]] = null
  var attributes = tables.flatMap(_.attributes).toSet.toArray

  def init(): this.type =
    this.gpuData = buildData()
    this.gpuDataRows = buildRows()
    this

  def getAttributes(): Array[Variable] =
    attributes

  def setAttributes(variables:Array[Variable]):this.type =
    attributes = variables
    this

  def getActive(attribute: Variable): Set[Int] =
    val domains = tables.zip(gpuDataRows).filter { case (table, rows) => table.contains(attribute)}
      .map { case (table, rows) => table.getActiveSet(rows, attribute)}.toSet
    if domains.isEmpty then Set() else domains.reduce(_ intersect _)

  def gpuTables(): Array[Array[Int]] =
    gpuData

  def gpuRows(): Array[Array[Int]] =
    gpuDataRows

  def setData(data: Array[Array[Int]]): this.type =
    gpuData = data
    this

  def setDataRows(result: Array[Array[Int]]): this.type =
    gpuDataRows = result
    this

  def newQuery(result: Array[Array[Int]]): GPUQuery =
    GPUQuery(tables)
      .setDataRows(result)
      .setData(gpuData)

  def colSize(): Array[Int] =
    tables.map(_.colSize)

  def rowSize(): Array[Int] =
    tables.map(_.rowSize)

  def attr(attribute: Variable): Array[Int] =
    tables.map(table => table.getIndex(attribute))

  protected def buildData(): Array[Array[Int]] =
    val maxData = tables.map(_.gpuData)
    val maxLength = maxData.map(_.length).max
    maxData.map(data=>{
      val rows =  Array.fill[Int](maxLength)(0)
      Array.copy(data, 0, rows, 0, data.length)
      rows
    })

  protected def buildRows(): Array[Array[Int]] =
    val maxData = tables.map(_.rows)
    val maxLength = maxData.map(_.length).max
    maxData.map(data=>{
      val rows =  Array.fill[Int](maxLength)(0)
      Array.copy(data, 0, rows, 0, data.length)
      rows
    })