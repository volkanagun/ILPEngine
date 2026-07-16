# SiLP: Scala Inductive Logic Programming Engine

## Description

SiLP is an inductive logic programming (ILP) framework implemented in Scala and Java. The repository contains executable source code for query execution, query optimization, unification, and predicate invention over Prolog-style datasets.

The main reproducibility entry points are command-line programs in executable source format:

- `src/ilp/commands/QueryCommand.java`
- `src/ilp/commands/PredicateInventionCommand.java`

Core implementation code is under `src/ilp/`, example datasets are under `examples/`, and supporting documentation is under `docs/`.

## Repository Layout

- `src/ilp/commands/`: executable command-line entry points
- `src/ilp/data/`: database, parser, predicate, optimization, and program structures
- `src/ilp/invent/`: predicate invention workflow and templates
- `src/ilp/experiments/`: experiment loading and parameter configuration
- `src/ilp/tests/`: test and example runner sources
- `examples/`: datasets used for query execution and learning experiments
- `docs/`: technical documentation and reproducibility notes
- `resources/`: cached data, experiment outputs, and bundled third-party binaries used in some experiments

## Dataset Information

Datasets are stored in `examples/<dataset-name>/`. Each dataset directory typically contains:

- `bk.pl`: background knowledge and primitive facts/rules
- `query.pl`: query program used for inference runs
- `exs.pl`: positive and negative examples used for learning runs
- `bias.pl`: predicate declarations and typing constraints
- `pi-meta-rules.pl`: predicate invention meta-rules when the dataset supports invention

Representative datasets used in the reproducibility guide:

- `examples/ptc/`
- `examples/pte/`

The repository includes datasets gathered from prior ILP benchmarks and public sources. Licensing may differ by dataset, so dataset-specific provenance should be retained when redistributing supplemental materials.

## Code Information

The repository mixes Scala and Java source files. The command-line workflows expected by the manuscript are implemented in executable source form:

- `src/ilp/commands/QueryCommand.java`
- `src/ilp/commands/PredicateInventionCommand.java`

These commands call into the main Scala implementation:

- `src/ilp/data/database/`
- `src/ilp/data/optimization/`
- `src/ilp/data/program/`
- `src/ilp/invent/`
- `src/ilp/experiments/`

Build configuration and runtime dependencies are defined in `pom.xml`.

## Requirements

To run the command-line workflows, use:

- Java 11 or newer
- Maven 3.x
- Scala runtime dependencies resolved through `pom.xml`

Dependencies currently declared in `pom.xml` include:

- Scala 3 standard library
- Scala parallel collections
- Scala parser combinators
- RoaringBitmap
- Apache Jena
- PostgreSQL JDBC
- Virtuoso JDBC

## Build Instructions

From the repository root:

1. Resolve runtime dependencies:

```bash
mvn package
```

This populates `target/lib/` with the dependency jars referenced by the command-line examples.

2. Compile the Java command entry points against the project classes and downloaded dependencies:

```bash
javac -cp 'out/production/ILPEngine:target/lib/*' \
  -d out/production/ILPEngine \
  src/ilp/commands/*.java
```

Notes:

- The repository expects the project classes to be available in `out/production/ILPEngine`, which is the default IntelliJ output directory used by the current project layout.
- If you build outside IntelliJ, compile the Scala and Java sources into a single classes directory before running the commands.

## Usage Instructions

### Query Execution

Run:

```bash
java -cp 'out/production/ILPEngine:target/lib/*' ilp.commands.QueryCommand \
  --example ptc \
  --query-file examples/ptc/query.pl \
  --engine parallel \
  --optimizer bellman-ford
```

Arguments:

- `--example <name>`: dataset directory under `examples/`
- `--query-file <path>`: query program file
- `--engine <name>`: `serial`, `parallel`, `bitmap-serial`, or `bitmap-parallel`
- `--optimizer <name>`: `none`, `iterative`, or `bellman-ford`
- `--recursion-depth <n>`: recursion depth limit; default `10`

The command prints:

- dataset and query file names
- selected engine and optimizer
- `loadTimeMs`
- `indexTimeMs`
- `queryTimeMs`
- `resultCount`
- variable bindings for each result

### Predicate Invention

Run:

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

Arguments:

- `--example <name>`: dataset directory under `examples/`
- `--exs-file <path>`: examples file containing `pos(...)` and `neg(...)`
- `--meta-rules-file <path>`: one meta-rule per line
- `--max-rules <n>`
- `--iterations <n>`
- `--window-size <n>`
- `--filter-size <n>`
- `--untested-size <n>`
- `--score-threshold <x>`
- `--recursion-depth <n>`

The command prints:

- dataset and input file names
- total runtime in milliseconds
- number of results
- positive and negative sample counts
- applied search parameters
- best learned program and score statistics

## Methodology

### Query Workflow

1. Load background knowledge from `bk.pl`.
2. Build database indexes and statistics.
3. Parse the query program from `query.pl`.
4. Select the execution engine and optimization strategy from the command line.
5. Execute the optimized query plan.
6. Print timings and substitutions for the query head variables.

### Predicate Invention Workflow

1. Load the selected example dataset.
2. Parse positive and negative samples from `exs.pl`.
3. Parse invention meta-rules from `pi-meta-rules.pl`.
4. Configure search limits and thresholds from the command line.
5. Run induction and select the best-scoring hypothesis.
6. Print the resulting program and score statistics.

## Data Preprocessing

No separate learned-feature preprocessing pipeline is required before running the provided SiLP examples.

The dataset preparation used by this repository is limited to file organization and parsing into the engine's Prolog-style input format:

1. Facts and rules are stored in plain text `.pl` files.
2. Query tasks are stored separately from training examples and bias declarations.
3. During execution, the loader parses these files and converts them into internal predicate, rule, and database objects.
4. Database indexes are then built in memory before query evaluation or predicate invention.

If a dataset uses custom functional predicates, corresponding predicate implementations and parser support must be added under `src/ilp/data/predicates/` and the parser/database loading path must recognize that syntax.

## Additional Documentation

- [Reproducibility guide](docs/Reproducibility.md)
- [Data structures](docs/Data%20structures.md)
- [Unification](docs/Unification.md)
- [Query optimization](docs/Query%20Optimization.md)
- [Predicate invention](docs/Predicate%20Invention.md)

## Citations

If this repository is used in a manuscript or benchmark study, cite:

- the SiLP / ILPEngine repository
- the associated manuscript that reports the results
- original sources for any redistributed benchmark datasets where required

## License and Contribution Guidelines

No repository-wide license file is currently included in this tree. If the supplemental package is submitted with a manuscript, add the intended license statement in the submission materials or repository metadata.

Contribution guidelines are informal in the current repository state. Contributors should preserve dataset structure, keep executable sources under `src/`, and update reproducibility documentation when changing command-line workflows.
