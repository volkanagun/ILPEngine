# Reproducibility

This document adds two runnable entry points under `ilp.commands`:

- `ilp.commands.QueryCommand`
- `ilp.commands.PredicateInventionCommand`

The query examples below use `ptc` and `pte`.

## Environment

- Java 11+ is required.
- The runtime classpath uses the existing compiled classes in `out/production/ILPEngine` and the dependency jars in `target/lib`.
- The new command sources live in `src/ilp/commands`.

## Compile the commands

Run this from the repository root:

```bash
javac -cp 'out/production/ILPEngine:target/lib/*' -d out/production/ILPEngine src/ilp/commands/*.java
```

## Query entry point

Entry point:

```text
ilp.commands.QueryCommand
```

Supported arguments:

- `--example <name>`: example folder name under `examples/`
- `--query-file <path>`: query program file
- `--engine <name>`: one of `serial`, `parallel`, `bitmap-serial`, `bitmap-parallel`
- `--optimizer <name>`: one of `none`, `iterative`, `bellman-ford`
- `--recursion-depth <n>`: recursive depth for the engine, default `10`

Reference query program files:

```text
examples/ptc/query.pl
examples/pte/query.pl
```

Example command for `ptc`:

```bash
java -cp 'out/production/ILPEngine:target/lib/*' ilp.commands.QueryCommand \
  --example ptc \
  --query-file examples/ptc/query.pl \
  --engine parallel \
  --optimizer bellman-ford
```

Example command for `pte`:

```bash
java -cp 'out/production/ILPEngine:target/lib/*' ilp.commands.QueryCommand \
  --example pte \
  --query-file examples/pte/query.pl \
  --engine bitmap-parallel \
  --optimizer iterative
```

What it prints:

- database loading time in milliseconds
- database indexing time in milliseconds
- query execution time in milliseconds
- result count
- result variable bindings for the query head variables
- the instantiated head predicate for each result

The query time does not include database loading or database indexing.

Engine options:

- `serial`: `EngineSerial`
- `parallel`: `EngineParallel`
- `bitmap-serial`: `EngineRoaringSerial`
- `bitmap-parallel`: `EngineRoaringParallel`

Optimizer options:

- `none`: `plan.optimizeNone(query)`
- `iterative`: `plan.optimizeExperimental(query)`
- `bellman-ford`: `plan.optimizeBellmanFord(query)`

## Predicate invention entry point

Entry point:

```text
ilp.commands.PredicateInventionCommand
```

Supported arguments:

- `--example <name>`: example folder name under `examples/`
- `--exs-file <path>`: examples file containing both `pos(...)` and `neg(...)`
- `--meta-rules-file <path>`: meta-rules file, one rule per line
- `--max-rules <n>`
- `--iterations <n>`
- `--window-size <n>`
- `--filter-size <n>`
- `--untested-size <n>`
- `--score-threshold <x>`
- `--recursion-depth <n>`

Reference files:

- examples file: `examples/pte/exs.pl`
- meta-rules: `examples/pte/pi-meta-rules.pl`

Example command:

```bash
java -cp 'out/production/ILPEngine:target/lib/*' ilp.commands.PredicateInventionCommand \
  --example pte \
  --exs-file examples/pte/exs.pl \
  --meta-rules-file examples/pte/pi-meta-rules.pl \
  --max-rules 20 \
  --iterations 2 \
  --window-size 2 \
  --filter-size 20 \
  --untested-size 20 \
  --score-threshold 0.7
```

The command reads the provided `exs.pl` file for both positive and negative examples, reads the meta-rules, runs predicate invention, and prints:

- total time in milliseconds
- result count
- positive and negative sample counts
- the configured execution parameters
- the best result program
- the best score, positive rate, and negative rate

The execution parameters are applied exactly through:

```scala
.setMaxRules(params.maxRules)
.setIter(params.iterationsSize)
.setWindow(params.windowSize)
.setFilterSize(params.filterSize)
.setUntestedSize(params.unTestedSize)
.setScoreThreshold(params.scoreThreshold)
```

## Notes

- The query command reads the program directly from the file given in `--query-file`.
- The predicate invention command expects one meta-rule per line in the file passed to `--meta-rules-file`.
- The predicate invention command expects the file passed to `--exs-file` to contain only `pos(...)` and `neg(...)` lines.
