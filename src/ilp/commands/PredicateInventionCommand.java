package ilp.commands;

import ilp.data.database.EngineParallel;
import ilp.data.predicates.Predicate;
import ilp.data.program.Hypothesis;
import ilp.data.program.Rule;
import ilp.experiments.Experiment;
import ilp.experiments.Params;
import ilp.invent.Binary;
import ilp.invent.Execution;
import ilp.invent.UnionBinary;
import scala.collection.immutable.Set;
import scala.jdk.javaapi.CollectionConverters;

import java.nio.file.Path;
import java.util.Comparator;
import java.util.Map;

public final class PredicateInventionCommand {

    private PredicateInventionCommand() {
    }

    public static void main(String[] rawArgs) throws Exception {
        Map<String, String> args = CommandSupport.parseArgs(rawArgs);

        String example = CommandSupport.require(args, "--example");
        String exsFile = CommandSupport.require(args, "--exs-file");
        String metaRulesFile = CommandSupport.require(args, "--meta-rules-file");

        Params params = new Params(example);
        params.maxRules_$eq(CommandSupport.getInt(args, "--max-rules", params.maxRules()));
        params.iterationsSize_$eq(CommandSupport.getInt(args, "--iterations", params.iterationsSize()));
        params.windowSize_$eq(CommandSupport.getInt(args, "--window-size", params.windowSize()));
        params.filterSize_$eq(CommandSupport.getInt(args, "--filter-size", params.filterSize()));
        params.unTestedSize_$eq(CommandSupport.getInt(args, "--untested-size", params.unTestedSize()));
        params.scoreThreshold_$eq(CommandSupport.getDouble(args, "--score-threshold", params.scoreThreshold()));
        params.recursionSize_$eq(CommandSupport.getInt(args, "--recursion-depth", params.recursionSize()));

        Experiment experiment = new Experiment(params).load();
        Map.Entry<Set<Predicate>, Set<Predicate>> samples = CommandSupport.parseExampleSamples(Path.of(exsFile));
        Set<Predicate> positives = samples.getKey();
        Set<Predicate> negatives = samples.getValue();
        Rule[] metaRules = CommandSupport.parseRuleFile(Path.of(metaRulesFile));

        EngineParallel engine = new EngineParallel(experiment.getDatabase(), params.recursionSize());
        Binary binary = new Binary(engine);
        binary.addMetaRule(metaRules);
        binary.setPositiveThreshold(params.binaryPositiveThreshold());
        binary.setNegativeThreshold(params.binaryNegativeThreshold());
        binary.setScoreThreshold(params.scoreThreshold());
        binary.setResembleThreshold(params.resembleThreshold());
        binary.setResembleWindow(params.resembleWindow());

        UnionBinary union = new UnionBinary(engine);
        union.setPositiveThreshold(params.unionPositiveThreshold());
        union.setNegativeThreshold(params.unionNegativeThreshold());
        union.setScoreThreshold(params.scoreThreshold());
        union.setResembleThreshold(params.resembleThreshold());
        union.setResembleWindow(params.resembleWindow());

        Execution execution = new Execution(engine)
                .setPositives(positives)
                .setNegatives(negatives)
                .setMaxRules(params.maxRules())
                .setIter(params.iterationsSize())
                .setWindow(params.windowSize())
                .setFilterSize(params.filterSize())
                .setUntestedSize(params.unTestedSize())
                .setScoreThreshold(params.scoreThreshold())
                .addTemplate(binary)
                .addTemplate(union)
                .compile();

        long startNanos = System.nanoTime();
        Set<Hypothesis> results = execution.induction();
        double elapsedMillis = (System.nanoTime() - startNanos) / 1_000_000.0;

        System.out.println("example=" + example);
        System.out.println("exsFile=" + exsFile);
        System.out.println("metaRulesFile=" + metaRulesFile);
        System.out.println("engine=" + engine.getClass().getSimpleName());
        System.out.printf("timeMs=%.3f%n", elapsedMillis);
        System.out.println("resultCount=" + results.size());
        System.out.println("positives=" + positives.size());
        System.out.println("negatives=" + negatives.size());
        System.out.println("parameters:");
        System.out.println(".setMaxRules(" + params.maxRules() + ")");
        System.out.println(".setIter(" + params.iterationsSize() + ")");
        System.out.println(".setWindow(" + params.windowSize() + ")");
        System.out.println(".setFilterSize(" + params.filterSize() + ")");
        System.out.println(".setUntestedSize(" + params.unTestedSize() + ")");
        System.out.println(".setScoreThreshold(" + params.scoreThreshold() + ")");

        Hypothesis best = CollectionConverters.asJava(results).stream()
                .max(Comparator.comparingDouble(Hypothesis::getScore))
                .orElseThrow(() -> new IllegalStateException("No predicate invention result was produced."));

        System.out.println("bestProgram:");
        System.out.println(best.normalize());
        System.out.printf("bestScore=%.6f%n", best.getScore());
        System.out.printf("bestPositiveRate=%.6f%n", best.getPosRate());
        System.out.printf("bestNegativeRate=%.6f%n", best.getNegRate());
    }
}
