#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

use Getopt::Long;
Getopt::Long::Configure("pass_through");

use Pod::Usage;
use FindBin;
use lib $FindBin::Bin;

# command-line ptions
my $output_dir = "results";
my $runner = "docker";
my $container = "circuitous:latest";
my $nocache;
my $tabulate;
my $verbose;
my $help;

my @unparsed_options;

GetOptions(
    'output=s'      => \$output_dir,
    'runner=s'      => \$runner,
    'container=s'   => \$container,
    'verbose'       => \$verbose,
    'no-cache'      => \$nocache,
    'tabulate'      => \$tabulate,
    'help'          => \$help,
) or pod2usage(2);

# Get unparsed options (if any)
@unparsed_options = @ARGV;

# Display help message and exit if the help option is used
pod2usage(1) if $help;

=head1 NAME

benchmark.pl - A script to build and execute circuitous benchmarks in a container.

=head1 SYNOPSIS

benchmark.pl [options]

Runner options:

  --output <folder>     Output file for results
  --runner <string>     Runner (Default: "docker")
  --container <string>  Container name (Default: "circuitous:latest")
  --no-cache            To build container withour cache.
  --tabulate            Print results table on-the-fly.
  --verbose             Enable verbose mode
  --help                Display this help message

Forwarded benchmark options:

  --benchmark_list_tests={true|false}
  --benchmark_filter=<regex>
  --benchmark_min_time=`<integer>x` OR `<float>s`
  --benchmark_min_warmup_time=<min_warmup_time>
  --benchmark_repetitions=<num_repetitions>
  --benchmark_enable_random_interleaving={true|false}
  --benchmark_report_aggregates_only={true|false}
  --benchmark_display_aggregates_only={true|false}
  --benchmark_format=<console|json|csv>
  --benchmark_out=<filename>
  --benchmark_out_format=<json|console|csv>
  --benchmark_color={auto|true|false}
  --benchmark_context=<key>=<value>,...
  --benchmark_time_unit={ns|us|ms|s}

=cut

sub build {
    my ($cmd) = @_;
    if ($verbose) {
        say "Building: $cmd";
    }

    my $output = `$cmd`;

    if ($verbose && $? == 0) {
        say "Docker built successfully.";
    } else {
        say "Error building docker: $?";
    }
}

sub run {
    my ($cmd) = @_;
    if ($verbose) {
        say "Running: $cmd";
    }

    my $output = `$cmd`;
}

sub main {
    my $script_dir = $FindBin::Bin;

    my $build_cmd = "$runner build -t $container $script_dir";

    if (defined $nocache) {
        $build_cmd .= " --no-cache";
    }

    build($build_cmd);

    my $run_cmd = "$runner run -v $output_dir:/tmp/output -dt $container" . " " . join(" ", @unparsed_options);

    if ($tabulate) {
        $run_cmd .= "--benchmark_counters_tabular=true"
    }

    run($run_cmd);
}

main();
