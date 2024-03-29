# Circuitous Benchmarking Container

This container serves as an environment for evaluating circuit compilation times
and circuit sizes, with a primary emphasis on circuit size assessment. It
collects data on the amount of primitives within the Circuitous IR and produced
Verilog. You can find the benchmark set and measured results and helper scripts
in a separate repository: [Circuitous Benchmarks](https://github.com/trailofbits/circuitous-benchmarks/).

## How to Run?

To run the benchmarks, use the following helper script to build and execute them
within a Docker container:

```bash
$ ./benchmark.pl
```

The `benchmark.pl` runner provides various configuration options, including:

```bash
  --output <folder>     Output file for results.
  --runner <string>     Runner (Default: "docker").
  --tag                 Tag results by name (Default: current timestamp).
  --container <string>  Container name (Default: "circuitous:latest").
  --no-cache            Build the container without cache.
  --list                List all available benchmarks.
  --tabulate            Print results table on-the-fly.
  --filter <regex>      Filter benchmarks by regex.
  --verbose             Enable verbose mode.
  --help                Display this help message.
```

It serves as a wrapper around the Google Benchmark driver, and any unrecognized
options are forwarded to the driver. The driver recognizes the following
options:

```bash
  --benchmark_min_time=<integer>x OR <float>s
  --benchmark_min_warmup_time=<min_warmup_time>
  --benchmark_repetitions=<num_repetitions>
  --benchmark_enable_random_interleaving={true|false}
  --benchmark_report_aggregates_only={true|false}
  --benchmark_display_aggregates_only={true|false}
  --benchmark_format=<console|json|csv>
  --benchmark_color={auto|true|false}
  --benchmark_context=<key>=<value>,...
  --benchmark_time_unit={ns|us|ms|s}
```

A common use case is to run minimal or large benchmarks, and you can filter them
by prefix:

```bash
$ ./benchmark.pl --filter min
```

When you execute this command, it will create a folder named `results`. You can
customize the folder name by specifying the `--output` option. Within this folder,
you will find a file named `<tag>.json`, which contains the accumulated results.
Additionally, there will be folders containing the generated Verilog and BLIFF
files. You can set a custom tag for the `<tag>` part by using the `--tag` option. By
default, the tag is set to the current timestamp.

NOTE: The script does not check for the existence of previous files. Be
cautious, as it can overwrite previous results.

To monitor benchmark results in real-time, use the `--tabulate` option, which will
print results to the terminal as benchmarks finish.

## Comparing results

Results generated by circuitous benchmarks are compatible with Google Benchmarks
tooling. Therefore, you can use the comparison script provided by Google
Benchmarks, which is available at
[Google Benchmarks Compare Script](https://github.com/google/benchmark/blob/main/tools/compare.py), as follows:

```bash
python compare.py <tag-1>.json <tag-2>.json
```
