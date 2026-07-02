package ilp.commands;

import ilp.data.database.Database;
import ilp.data.database.Engine;
import ilp.data.database.EngineParallel;
import ilp.data.database.EngineRoaringParallel;
import ilp.data.database.EngineRoaringSerial;
import ilp.data.database.EngineSerial;
import ilp.data.optimization.Optimized;
import ilp.data.optimization.Plan;
import ilp.data.predicates.Predicate;
import ilp.data.program.Hypothesis;
import ilp.data.program.Substitution;
import ilp.data.variables.Variable;
import scala.collection.immutable.Set;
import scala.jdk.javaapi.CollectionConverters;

import java.nio.file.Path;
import java.util.Map;

public final class QueryCommand {

    private QueryCommand() {
    }

    public static void main(String[] rawArgs) throws Exception {
        Map<String, String> args = CommandSupport.parseArgs(rawArgs);

        String example = CommandSupport.require(args, "--example");
        String queryFile = CommandSupport.require(args, "--query-file");
        String engineName = args.getOrDefault("--engine", "serial");
        String optimizerName = args.getOrDefault("--optimizer", "iterative");
        int recursionDepth = CommandSupport.getInt(args, "--recursion-depth", 10);

        Path exampleDir = Path.of("examples", example);

        long loadStartNanos = System.nanoTime();
        Database database = CommandSupport.loadDatabase(exampleDir, example);
        double loadMillis = (System.nanoTime() - loadStartNanos) / 1_000_000.0;

        long indexingStartNanos = System.nanoTime();
        database.build();
        double indexingMillis = (System.nanoTime() - indexingStartNanos) / 1_000_000.0;

        Hypothesis query = CommandSupport.parseHypothesisFile(Path.of(queryFile));

        Plan plan = new Plan(database, true);
        Engine engine = createEngine(database, engineName, recursionDepth);

        long queryStartNanos = System.nanoTime();
        Optimized[] optimized = optimize(plan, query, optimizerName);
        Set<Substitution> results = engine.join(optimized, new Substitution());
        double queryMillis = (System.nanoTime() - queryStartNanos) / 1_000_000.0;

        System.out.println("example=" + example);
        System.out.println("queryFile=" + queryFile);
        System.out.println("engine=" + engineName);
        System.out.println("optimizer=" + optimizerName);
        System.out.printf("loadTimeMs=%.3f%n", loadMillis);
        System.out.printf("indexTimeMs=%.3f%n", indexingMillis);
        System.out.printf("queryTimeMs=%.3f%n", queryMillis);
        System.out.println("resultCount=" + results.size());

        Predicate head = query.head();
        Variable[] variables = head.getVariables();
        int index = 1;
        for (Substitution substitution : CollectionConverters.asJava(results)) {
            StringBuilder builder = new StringBuilder();
            builder.append("result[").append(index++).append("]: ");
            for (int position = 0; position < variables.length; position++) {
                Variable variable = variables[position];
                String value = substitution.valueByVariable(variable).isDefined()
                        ? substitution.valueByVariable(variable).get().toValue()
                        : "unbound";
                if (position > 0) {
                    builder.append(", ");
                }
                builder.append(variable.getName()).append("=").append(value);
            }
            builder.append(" -> ").append(query.callHead(substitution));
            System.out.println(builder);
        }
    }

    private static Optimized[] optimize(Plan plan, Hypothesis query, String optimizerName) {
        return switch (optimizerName) {
            case "none" -> plan.optimizeNone(query);
            case "bellman-ford" -> plan.optimizeBellmanFord(query);
            case "iterative" -> plan.optimizeExperimental(query);
            default -> throw new IllegalArgumentException("Unknown optimizer: " + optimizerName
                    + ". Expected one of: none, iterative, bellman-ford");
        };
    }

    private static Engine createEngine(Database database, String engineName, int recursionDepth) {
        return switch (engineName) {
            case "serial" -> new EngineSerial(database, recursionDepth);
            case "parallel" -> new EngineParallel(database, recursionDepth);
            case "bitmap-serial" -> new EngineRoaringSerial(database, recursionDepth);
            case "bitmap-parallel" -> new EngineRoaringParallel(database, recursionDepth);
            default -> throw new IllegalArgumentException("Unknown engine: " + engineName
                    + ". Expected one of: serial, parallel, bitmap-serial, bitmap-parallel");
        };
    }
}
