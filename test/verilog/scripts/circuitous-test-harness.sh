#!/bin/sh

# Nothing can produce an error
set -e

verilog_circuit=$1
trace_file=$2

root_name=`basename $verilog_circuit ".v"`

tv_circuit=$root_name.e2e.tv
ruby external/e2egen $trace_file > $tv_circuit

tbgen_config=$root_name.tb.yml

# Now ninja parse the inputs and outputs
current_name=`sed "2q;d" $verilog_circuit | cut -d ' ' -f 3 | cut -d ',' -f 1`
current_raw_size=`sed "2q;d" $verilog_circuit | cut -d ' ' -f 2 | cut -d ':' -f 1 | cut -d '[' -f 2`
current_size=`expr $current_raw_size + 1`


next_name=`sed "3q;d" $verilog_circuit | cut -d ' ' -f 3 | cut -d ',' -f 1`
next_raw_size=`sed "3q;d" $verilog_circuit | cut -d ' ' -f 2 | cut -d ':' -f 1 | cut -d '[' -f 2`
next_size=`expr $next_raw_size + 1`

out_name=`sed "5q;d" $verilog_circuit | cut -d ' ' -f 3 | cut -d ',' -f 1`
out_raw_size=`sed "5q;d" $verilog_circuit | cut -d ' ' -f 2 | cut -d ':' -f 1 | cut -d '[' -f 2`
out_size=`expr $out_raw_size + 1`

tb_circuit=$root_name.tb.gen.v

cat > $tbgen_config << EOL
module: circuit
file: ./$verilog_circuit
tb_file: $tb_circuit
tv_file: $tv_circuit
inputs:
 - "$current_size $current_name"
 - "$next_size $next_name"
outputs:
 - "$out_size $out_name"

vector_space: []

extra_vectors: $tv_circuit
EOL

ruby external/tbgen $tbgen_config

if ! grep -q "readmemb(.*$tv_circuit.*test_vectors" $tb_circuit; then
    echo "Failed grep check of readmemb macro"
    exit 1
fi

vpp_file=$root_name.tb.vpp
iverilog -o $vpp_file $tb_circuit $verilog_circuit
ruby external/run-tests $vpp_file
exit $?
