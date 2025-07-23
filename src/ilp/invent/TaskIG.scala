package ilp.invent

import ilp.data.Hypothesis
import ilp.data.predicates.Predicate
import org.apache.ignite.lang.IgniteRunnable

import java.util.concurrent.Callable

class TaskIG(val source:Hypothesis, val targets:Array[Hypothesis], val head:Predicate, val templateIG: TemplateIG) extends Callable[(Set[Hypothesis],Array[Hypothesis])] with Serializable{

  override def call(): (Set[Hypothesis], Array[Hypothesis]) =
    templateIG.compute(source, targets, head)

}
