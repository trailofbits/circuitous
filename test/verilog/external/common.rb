# routines and constants common to both `tbgen` and `tvgen`

HELP = !!ARGV.delete("-h") || !!ARGV.delete("--help")
VERBOSE = !!ARGV.delete("--verbose") || ENV.key?("VERBOSE") || ENV.key?("V")

# TODO(ww): Remove this. We shouldn't limit the number of test vectors artificially here.
MAX_TEST_VECTORS = 1024

Wire = Struct.new(:name, :size) do
  def width_spec
    "[#{size - 1}:0]"
  end
end

def verbose(msg)
  $stderr.puts "[+] #{msg}" if VERBOSE
end

def barf(msg)
  $stderr.puts "Fatal: #{msg}"
  exit 1
end

def fixup_spec!(spec_dir, spec)
  unless spec.key?(:tb_file)
    spec[:tb_file] = "#{spec[:module]}.tb.gen.v"
  end

  unless spec.key?(:tv_file)
    spec[:tv_file] = "#{spec[:module]}.tv"
  end

  [:file, :tv_file, :tb_file].each do |rel_file|
    spec[rel_file] = spec_dir / spec[rel_file]
  end

  barf "file: #{spec[:file]} must be a file" unless spec[:file].file?

  spec[:inputs].map! { |i| parse_wire i }
  spec[:outputs].map! { |o| parse_wire o }

  verbose "fixed up: #{spec}"
end

def parse_wire(wire)
  size, name = wire.split " ", 2
  Wire.new name, size.to_i
end

def comment(io, thing)
  io.puts "// #{thing}"
end
