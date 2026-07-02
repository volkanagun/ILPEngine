package ilp.commands;

import ilp.data.database.Bias;
import ilp.data.database.Database;
import ilp.data.predicates.Predicate;
import ilp.data.program.Hypothesis;
import ilp.data.program.Parser;
import ilp.data.program.Rule;
import scala.Option;
import scala.collection.immutable.Set;
import scala.jdk.javaapi.CollectionConverters;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.AbstractMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

final class CommandSupport {

    private CommandSupport() {
    }

    static Map<String, String> parseArgs(String[] args) {
        Map<String, String> values = new LinkedHashMap<>();
        for (int index = 0; index < args.length; index++) {
            String arg = args[index];
            if (!arg.startsWith("--")) {
                throw new IllegalArgumentException("Unexpected argument: " + arg);
            }

            if (index + 1 < args.length && !args[index + 1].startsWith("--")) {
                values.put(arg, args[++index]);
            } else {
                values.put(arg, "true");
            }
        }
        return values;
    }

    static boolean hasFlag(Map<String, String> args, String key) {
        return Boolean.parseBoolean(args.getOrDefault(key, "false"));
    }

    static String require(Map<String, String> args, String key) {
        String value = args.get(key);
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Missing required argument: " + key);
        }
        return value;
    }

    static int getInt(Map<String, String> args, String key, int defaultValue) {
        return args.containsKey(key) ? Integer.parseInt(args.get(key)) : defaultValue;
    }

    static double getDouble(Map<String, String> args, String key, double defaultValue) {
        return args.containsKey(key) ? Double.parseDouble(args.get(key)) : defaultValue;
    }

    static List<String> readProgramLines(Path path) throws IOException {
        List<String> lines = new ArrayList<>();
        for (String raw : Files.readAllLines(path)) {
            String line = raw.trim();
            if (!line.isEmpty() && !line.startsWith("%")) {
                lines.add(line);
            }
        }
        return lines;
    }

    static Hypothesis parseHypothesisFile(Path path) throws IOException {
        List<Rule> rules = new ArrayList<>();
        for (String line : readProgramLines(path)) {
            rules.add(getOrThrow(Parser.parseRule(line), "rule", line));
        }
        if (rules.isEmpty()) {
            throw new IllegalArgumentException("No rules found in " + path);
        }
        return new Hypothesis(rules.toArray(new Rule[0])).build();
    }

    static Rule[] parseRuleFile(Path path) throws IOException {
        List<Rule> rules = new ArrayList<>();
        for (String line : readProgramLines(path)) {
            rules.add(getOrThrow(Parser.parseRule(line), "meta-rule", line));
        }
        if (rules.isEmpty()) {
            throw new IllegalArgumentException("No rules found in " + path);
        }
        return rules.toArray(new Rule[0]);
    }

    static Database loadDatabase(Path exampleDir, String databaseName) throws IOException {
        Database database = new Database(databaseName);
        Path bkFile = exampleDir.resolve("bk.pl");
        for (String line : readProgramLines(bkFile)) {
            if (!line.contains(":-")) {
                database.add(getOrThrow(Parser.parsePredicate(line), "database fact", line));
            }
        }
        for (String line : readProgramLines(bkFile)) {
            if (line.contains(":-")) {
                database.add(getOrThrow(Parser.parseHypothesis(line), "database primitive", line));
            }
        }
        database.setBias(new Bias().build(exampleDir.resolve("bias.pl").toString()));
        return database;
    }

    static Set<Predicate> parseSamples(Path path, String wrapper) throws IOException {
        LinkedHashSet<Predicate> results = new LinkedHashSet<>();
        for (String line : readProgramLines(path)) {
            String normalized = line;
            String prefix = wrapper + "(";
            if (normalized.startsWith(prefix) && normalized.endsWith(").")) {
                normalized = normalized.substring(prefix.length(), normalized.length() - 2) + ".";
            }
            results.add(getOrThrow(Parser.parsePredicate(normalized), wrapper + " sample", line));
        }
        return CollectionConverters.asScala(results).toSet();
    }

    static Map.Entry<Set<Predicate>, Set<Predicate>> parseExampleSamples(Path path) throws IOException {
        LinkedHashSet<Predicate> positives = new LinkedHashSet<>();
        LinkedHashSet<Predicate> negatives = new LinkedHashSet<>();

        for (String line : readProgramLines(path)) {
            if (line.startsWith("pos(")) {
                positives.add(getOrThrow(Parser.parsePredicate(line.substring(4, line.length() - 2) + "."), "positive sample", line));
            } else if (line.startsWith("neg(")) {
                negatives.add(getOrThrow(Parser.parsePredicate(line.substring(4, line.length() - 2) + "."), "negative sample", line));
            } else {
                throw new IllegalArgumentException("Expected pos(...) or neg(...) line in " + path + ": " + line);
            }
        }

        return new AbstractMap.SimpleEntry<>(
                CollectionConverters.asScala(positives).toSet(),
                CollectionConverters.asScala(negatives).toSet()
        );
    }

    static <T> T getOrThrow(Option<T> value, String label, String source) {
        if (value.isDefined()) {
            return value.get();
        }
        throw new IllegalArgumentException("Could not parse " + label + ": " + source);
    }
}
