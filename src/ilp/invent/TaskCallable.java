package ilp.invent;

import ilp.data.predicates.Predicate;
import ilp.data.program.Hypothesis;
import scala.Array;
import scala.Tuple2;
import scala.collection.immutable.Set;

import java.util.concurrent.Callable;
import scala.jdk.CollectionConverters.*;
public class TaskCallable implements Callable<TaskResult> {
    //private TemplateFast template;
    private Hypothesis source;
    private Hypothesis[] targets;
    private Predicate targetPredicate;

    public TaskCallable(TemplateFast template, Hypothesis source, Hypothesis[] targets, Predicate targetPredicate) {
        //this.template = template;
        this.source = source;
        this.targets = targets;
        this.targetPredicate = targetPredicate;
    }

    @Override
    public TaskResult call() throws Exception {
        //Tuple2<Hypothesis[], Hypothesis[]> result = template.computeRemote(source, targets, targetPredicate);
        //return new TaskResult(result._1, result._2);

        return new TaskResult(new Hypothesis[0], new Hypothesis[0]);
    }
}
