package ilp.concepts

import ilp.data.Hypothesis

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class Execution:

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
    var sourceHypothesis = templates.par.flatMap(template => template.invent())
      .toArray
      .toSet
    var isFinished = sourceHypothesis.exists(_.isFinished())
    var count = 1
    while (!isFinished && sourceHypothesis.nonEmpty && count < iteration) do
      sourceHypothesis = templates.map(template => template.setSources(sourceHypothesis)).flatMap(template => template.invent())
        .toSet
      isFinished = sourceHypothesis.exists(item=> item.isFinished() && item.isComplete())
      count += 1

    sourceHypothesis.filter(item=> item.isComplete() && item.isFinished())
