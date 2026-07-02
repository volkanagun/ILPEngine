# Reproducibility

## Title

SiLP Reproducibility Guide

## Description

This document describes how to reproduce command-line query execution and predicate invention runs for the SiLP project. The executable source code for these workflows is in the `ilp.commands` package:

- `src/ilp/commands/QueryCommand.java`
- `src/ilp/commands/PredicateInventionCommand.java`

The guide keeps the workflow in executable form and gives concrete example datasets, commands, options, and expected outputs.

## Dataset Information

The project datasets are stored under `examples/`. Each dataset directory typically contains:

- `bk.pl`: background facts and primitive rules
- `query.pl`: query program
- `exs.pl`: positive and negative examples for learning
- `bias.pl`: predicate declarations and typing information

This reproducibility guide uses:

- `examples/ptc/`
- `examples/pte/`

Referenced files:

- query files:
  - `examples/ptc/query.pl`
  - `examples/pte/query.pl`
- predicate invention examples file:
  - `examples/pte/exs.pl`
- predicate invention meta-rules file:
  - `examples/pte/pi-meta-rules.pl`

## Code Information

The reproducibility entry points are:

- `ilp.commands.QueryCommand`
- `ilp.commands.PredicateInventionCommand`

Supporting executable code is under:

- `src/ilp/commands/`
- `src/ilp/data/database/`
- `src/ilp/data/optimization/`
- `src/ilp/invent/`

The runtime classpath uses:

- compiled project classes in `out/production/ILPEngine`
- dependency jars in `target/lib`

## Requirements

- Java 11 or newer
- compiled project classes in `out/production/ILPEngine`
- dependency jars in `target/lib`

Compile the command-line entry points from the repository root:

```bash
javac -cp 'out/production/ILPEngine:target/lib/*' -d out/production/ILPEngine src/ilp/commands/*.java
```

## Usage Instructions

### Query Command

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

What the query command prints:

- `loadTimeMs`: database loading time in milliseconds
- `indexTimeMs`: database indexing time in milliseconds
- `queryTimeMs`: query execution time in milliseconds
- `resultCount`
- result variable bindings for the query head variables
- instantiated head predicates

The query time does not include database loading or indexing time.

Engine choices:

- `serial`: `EngineSerial`
- `parallel`: `EngineParallel`
- `bitmap-serial`: `EngineRoaringSerial`
- `bitmap-parallel`: `EngineRoaringParallel`

Optimizer choices:

- `none`: `plan.optimizeNone(query)`
- `iterative`: `plan.optimizeExperimental(query)`
- `bellman-ford`: `plan.optimizeBellmanFord(query)`

### Predicate Invention Command

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

What the predicate invention command prints:

- total execution time in milliseconds
- result count
- positive and negative sample counts
- configured execution parameters
- best result program
- best score, positive rate, and negative rate

The execution parameters are applied through:

```scala
.setMaxRules(params.maxRules)
.setIter(params.iterationsSize)
.setWindow(params.windowSize)
.setFilterSize(params.filterSize)
.setUntestedSize(params.unTestedSize)
.setScoreThreshold(params.scoreThreshold)
```

## Methodology

### Query Execution

1. Load facts and primitive rules from `bk.pl`.
2. Build database indexes and statistics with `database.build()`.
3. Parse the query program from `query.pl`.
4. Select the engine from the command-line `--engine` option.
5. Select the optimizer from the command-line `--optimizer` option.
6. Run the join pipeline and print timings and result bindings.

### Predicate Invention

1. Load the example dataset selected by `--example`.
2. Parse positive and negative examples from the provided `exs.pl` file.
3. Parse meta-rules from the provided meta-rule file.
4. Configure predicate invention parameters from the command line.
5. Run invention templates and print the best resulting program.

## Verified Runs

Verified query run for `ptc`:

```text
loadTimeMs=3775.336
indexTimeMs=215.668
queryTimeMs=1352.328
```

Verified query run for `pte`:

```text
loadTimeMs=3414.385
indexTimeMs=387.489
queryTimeMs=754.211
```

Verified predicate invention run for `pte`:

```text
timeMs=94255.114
positives=162
negatives=136
```

## Citations

Please cite the SiLP project and any associated research articles that use this codebase. The repository README already points readers to Google Scholar references related to the project.

## License & Contribution Guidelines

This document does not define additional license or contribution terms beyond the repository’s existing project files. Follow the repository’s current conventions for code changes and documentation updates.

## Notes

- The query command reads the program directly from the file given in `--query-file`.
- The predicate invention command expects one meta-rule per line in the file passed to `--meta-rules-file`.
- The predicate invention command expects the file passed to `--exs-file` to contain only `pos(...)` and `neg(...)` lines.
