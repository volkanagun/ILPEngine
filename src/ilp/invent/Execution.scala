package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}

class Execution(var engine: Engine):


  var templates = Array[Template]()
  var iteration = 1

  def setTemplates(templates:Array[Template]):this.type =
    this.templates = templates
    this

  def setIter(iteration:Int):this.type =
    this.iteration = iteration
    this

  def addTemplate(template:Template):this.type =
    this.templates :+= template
    this

  def induction(): Set[Hypothesis] =
    var sourceHypothesis = templates.par
      .flatMap(template => template.invent()
        .map(hypothesis=> template.igRoaring(hypothesis)))
      .toArray.toSet

    sourceHypothesis = sourceHypothesis.filter(engine.validHypothesis)
    var isFinished = sourceHypothesis.exists(_.isFinished())
    var count = 1
    while (!isFinished && sourceHypothesis.nonEmpty && count < iteration) do
      println(s"Iteration: ${count} with size: ${sourceHypothesis.size}")
      sourceHypothesis = templates.map(template => template.setSources(sourceHypothesis).setTarget(sourceHypothesis.toArray))
        .flatMap(template => template.invent().par.map(hypothesis=> template.igRoaring(hypothesis)))
        .toSet
      sourceHypothesis = sourceHypothesis.filter(engine.validHypothesis).map(_.compact())
      //sourceHypothesis.toArray.foreach(h=>{h.print(); println("========")})

      isFinished = sourceHypothesis.exists(item=> item.isFinished() && item.isComplete())
      count += 1

    sourceHypothesis.filter(item=> item.isComplete() && item.isFinished())
