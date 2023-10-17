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
my $verbose;
my $help;

my @unparsed_options;

GetOptions(
    'output=s'      => \$output_dir,
    'runner=s'      => \$runner,
    'container=s'   => \$container,
    'verbose'       => \$verbose,
    'no-cache'      => \$nocache,
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

Options:

  --output <folder>     Output file for results
  --runner <string>     Runner (Default: "docker")
  --container <string>  Container name (Default: "circuitous:latest")
  --no-cache            To build container withour cache.
  --verbose             Enable verbose mode
  --help                Display this help message

=cut

sub build {
    my ($cmd) = @_;
    if ($verbose) {
        say "Building: $cmd";
    }

    my $output = `$cmd`;

    if ($verbose && $? == 0) {
        say "Docker built successfully.";
        say "Output:\n$output";
    } else {
        say "Error building docker: $?";
    }
}

sub run {
    my ($cmd) = @_;
    if ($verbose) {
        say "Running: $cmd";
    }
}

sub main {
    my $script_dir = $FindBin::Bin;

    my $build_cmd = "$runner build -t $container $script_dir";

    if (defined $nocache) {
        $build_cmd .= " --no-cache";
    }

    build($build_cmd);

    my $run_cmd = "$runner run -v $output_dir:/tmp/output -dt $container" . " " . join(" ", @unparsed_options);

    run($run_cmd);
}

main();
