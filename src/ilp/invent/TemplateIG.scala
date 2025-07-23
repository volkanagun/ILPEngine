package ilp.invent

import ilp.data.Hypothesis
import ilp.data.database.Engine
import ilp.data.predicates.Predicate
import org.apache.ignite.{Ignite, Ignition}
import org.apache.ignite.configuration.IgniteConfiguration
import org.apache.ignite.lang.IgniteRunnable
import org.apache.ignite.marshaller.jdk.JdkMarshaller
import org.apache.ignite.spi.discovery.tcp.TcpDiscoverySpi
import org.apache.ignite.spi.discovery.tcp.ipfinder.multicast.TcpDiscoveryMulticastIpFinder

import java.util
import java.util.Collections
import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.jdk.CollectionConverters.IterableHasAsJava

abstract class TemplateIG(engine: Engine) extends Template(engine) {

  def compute(source: Hypothesis, targets: Array[Hypothesis], targetPredicate: Predicate): (Set[Hypothesis], Array[Hypothesis]) = {
    val crrResults = inventNext(source, targets)

    val validResults = crrResults.par.map(hypothesis => {
        hypothesis.build().compact()
      }).filter(hypothesis => hypothesis.getRules().length < maxRules)
      .filter(hypothesis => engine.validHypothesis(hypothesis)).toArray

    val scoredResults = validResults.filter(_.validAritry(targetPredicate))
      .map(hypothesis => igCache(hypothesis))
      .filter(hypothesis => hypothesis.acceptNegRate(negThreshold) && hypothesis.acceptPosRate(posThreshold))

    val combineSet = scoredResults.toSet ++
      validResults.filter(result => !scoredResults.contains(result))

    (combineSet, scoredResults)
  }

  def computeRemote(targets: Array[Hypothesis], targetPredicate: Predicate): (Set[Hypothesis], Array[Hypothesis]) = {
    (Set(), Array())
  }


  override def invent(): Set[Hypothesis] =

    var doStop = false
    val targetHead = positives.head
    var finalResults = Set[Hypothesis]()
    var newResults = Set[Hypothesis]()
    val targets = target()

    val tasks = sources.par.map(source=> compute(source, targets, targetHead.copy().asPredicate()))
      .toArray
      .iterator


    while tasks.hasNext && !doStop do
      val (combineSet, scoredResults) = tasks.next()
      finalResults ++= combineSet
      doStop = stopCondition(scoredResults)
      newResults = scoredResults.toSet


    if doStop then newResults
    else {
      finalResults
    }


}
