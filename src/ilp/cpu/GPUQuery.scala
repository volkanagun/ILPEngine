package ilp.cpu

import ilp.data.predicates.Predicate
import ilp.data.variables.Variable

import java.text.AttributedCharacterIterator.Attribute
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class GPUQuery(var id:Int, var maxValue:Int, var tables: Array[GPUTable]):

  var gpuData: Array[Array[Int]] = null
  var gpuDataRows: Array[Array[Int]] = null
  var gpuIntersectedRows: Array[Int] = null

  var attributes = tables.flatMap(_.attributes).toSet.toArray

  def init(): this.type =
    this.gpuData = buildData()
    this.gpuDataRows = buildRows()
    this

  def getBody():Array[Predicate] =
    tables.map(_.predicate)

  def getAttributes(): Array[Variable] =
    attributes

  def getAttributeIds():Array[Int]=
    attributes.map(variable=>{
      id * 7 + variable.hashCode()
    })

  def getAttributeId(attribute: Variable):Int=
    id * 7 + attribute.hashCode()

  def newMap(map:Map[Int, Array[Array[Int]]]):Map[Int, Array[Array[Int]]]=
    tables.zip(gpuDataRows).map{case(table, rows) => {
      val id = table.pid
      val filters = map(id).zipWithIndex.filter{case(row, rowIndex)=>{
        rows(rowIndex) == 1
      }}.map(_._1)
      id -> filters
    }}.toMap

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

  def setIntersectedRows(result: Array[Int]): this.type =
    gpuIntersectedRows = result
    this

  def newQuery(result: Array[Array[Int]]): GPUQuery =
    GPUQuery(id, maxValue, tables)
      .setDataRows(result)
      .setData(gpuData)

  def newQuery(positions:Array[Int], result: Array[Array[Int]]): GPUQuery = {
    val clone = gpuDataRows.map(_.clone())
    positions.zip(result).foreach(indexPair => clone(indexPair._1) = indexPair._2)
    GPUQuery(id, maxValue, tables)
      .setDataRows(clone)
      .setData(gpuData)
  }

  def newIntersectedQuery(result: Array[Int]): GPUQuery =
    GPUQuery(id,maxValue, tables)
      .setIntersectedRows(result)
      .setData(gpuData)

  def valueMax():Int =
    maxValue

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

  override def toString: String = {
    val tableOrder = tables.map(_.name).mkString(" & ")
    val attributeOrder = attributes.map(_.name).mkString("[",",","]")
    tableOrder + "==>" + attributeOrder
  }

