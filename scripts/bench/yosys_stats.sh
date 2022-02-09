#!/usr/bin/env bash
# Copyright (c) 2022 Trail of Bits, Inc.

mkdir -p /tmp/yosys
cp $1 /tmp/yosys

cat > /tmp/yosys/my_library.lib << EOF
library(demo) {
	cell(NOT) {
		area: 2;
		pin(IN) { direction: input; }
		pin(OUT) { direction: output; function: "IN'"; }
	}
	cell(XOR) {
		area: 1;
		pin(A) { direction: input; }
		pin(B) { direction: input; }
		pin(OUT) { direction: output; function: "(A^B)"; }
	}
	cell(AND) {
		area: 100;
		preferred: false;
		pin(A) { direction: input; }
		pin(B) { direction: input; }
		pin(OUT) { direction: output; function: "(A&B)"; }
	}
	cell(BUF) {
		area: 1;
		pin(IN) { direction: input; }
		pin(OUT) { direction: output; function: "IN"; }
	}
}
EOF

echo "Preparing $2 from $1"
cat > /tmp/yosys/$1.ys << EOF
read_verilog $1;
hierarchy -check -top $2;
proc; opt; memory; opt; fsm; opt; techmap; opt;
flatten; opt -purge;
abc -liberty /tmp/yosys/my_library.lib;
opt;
read_liberty -lib /tmp/yosys/my_library.lib;
write_verilog netlist_$2_$1;
stat;
#shell
EOF

yosys /tmp/yosys/$1.ys

exit 0
