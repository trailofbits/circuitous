circuit = {
    "v7fbdb5f60d40": ["RESULT", 22, 1, "v7fbdb5f85590"],
    "v7fbdb5f63290": ["CONST_3_011", 0, 3],
    "v7fbdb5f801f0": ["CONST_2_11", 0, 2],
    "v7fbdb5f88740": ["CONST_1_0", 0, 1],
    "v7fbdb5f84670": ["CONST_2_00", 0, 2],
    "v7fbdb5f800b0": ["CONST_8_00000001", 0, 8],
    "v7fbdb5f6e9b0": ["CONST_32_00000000000000000000000011111111", 0, 32],
    "v7fbdb5f7c5b0": ["CONST_32_00000000000000000000000000000001", 0, 32],
    "v7fbdb5f7a0f0": ["CONST_32_00000000000000000000000000000000", 0, 32],
    "v7fbdb5f67470": ["CONST_32_00000000000000000000000000011111", 0, 32],
    "v7fbdb5f834a0": ["LLVM_add_32", 1, 32, 13, 42, "v7fbdb5f6af20", "v7fbdb5f69a90"],
    "v7fbdb5f86a50": [
        "LLVM_icmp_ult_1",
        1,
        1,
        53,
        36,
        "v7fbdb5f834a0",
        "v7fbdb5f69a90",
    ],
    "v7fbdb5f816d0": [
        "LLVM_icmp_ult_1",
        1,
        1,
        53,
        36,
        "v7fbdb5f834a0",
        "v7fbdb5f6af20",
    ],
    "v7fbdb5f721f0": ["LLVM_or_1", 1, 1, 29, 42, "v7fbdb5f86a50", "v7fbdb5f816d0"],
    "v7fbdb5f641f0": ["LLVM_and_32", 1, 32, 28, 42, "v7fbdb5f834a0", "v7fbdb5f6e9b0"],
    "v7fbdb5f62580": ["LLVM_and_32", 1, 32, 28, 42, "v7fbdb5f7a310", "v7fbdb5f7c5b0"],
    "v7fbdb5f64880": ["LLVM_icmp_eq_1", 1, 1, 53, 32, "v7fbdb5f62580", "v7fbdb5f7a0f0"],
    "v7fbdb5f720f0": ["LLVM_xor_32", 1, 32, 30, 42, "v7fbdb5f6af20", "v7fbdb5f69a90"],
    "v7fbdb5f67560": ["LLVM_xor_32", 1, 32, 30, 42, "v7fbdb5f720f0", "v7fbdb5f834a0"],
    "v7fbdb5f57640": ["LLVM_and_32", 1, 32, 28, 42, "v7fbdb5f67560", "v7fbdb5f7c5b0"],
    "v7fbdb5f79250": ["LLVM_icmp_ne_1", 1, 1, 53, 33, "v7fbdb5f57640", "v7fbdb5f7a0f0"],
    "v7fbdb5f65520": ["LLVM_icmp_eq_1", 1, 1, 53, 32, "v7fbdb5f834a0", "v7fbdb5f7a0f0"],
    "v7fbdb5f821c0": ["LLVM_lshr_32", 1, 32, 26, 42, "v7fbdb5f834a0", "v7fbdb5f67470"],
    "v7fbdb5f79540": ["LLVM_icmp_ne_1", 1, 1, 53, 33, "v7fbdb5f821c0", "v7fbdb5f7a0f0"],
    "v7fbdb5f783b0": ["LLVM_lshr_32", 1, 32, 26, 42, "v7fbdb5f69a90", "v7fbdb5f67470"],
    "v7fbdb5f6dfa0": ["LLVM_xor_32", 1, 32, 30, 42, "v7fbdb5f821c0", "v7fbdb5f783b0"],
    "v7fbdb5f655f0": ["LLVM_lshr_32", 1, 32, 26, 42, "v7fbdb5f6af20", "v7fbdb5f67470"],
    "v7fbdb5f5a020": ["LLVM_xor_32", 1, 32, 30, 42, "v7fbdb5f821c0", "v7fbdb5f655f0"],
    "v7fbdb5f701a0": ["LLVM_add_32", 1, 32, 13, 42, "v7fbdb5f6dfa0", "v7fbdb5f5a020"],
    "v7fbdb5f587b0": ["LLVM_icmp_eq_1", 1, 1, 53, 32, "v7fbdb5f701a0", "v7fbdb5f7c5b0"],
    "v7fbdb5f75920": ["EXTRACT_16_13", 2, 3, 16, 13, "v7fbdb5f69960"],
    "v7fbdb5f95520": ["EXTRACT_13_11", 2, 2, 13, 11, "v7fbdb5f69960"],
    "v7fbdb5f6b780": ["EXTRACT_11_10", 2, 1, 11, 10, "v7fbdb5f69960"],
    "v7fbdb5f7a510": ["EXTRACT_10_8", 2, 2, 10, 8, "v7fbdb5f69960"],
    "v7fbdb5f61390": ["EXTRACT_8_0", 2, 8, 8, 0, "v7fbdb5f69960"],
    "v7fbdb5f7a310": ["POPULATION_COUNT_32", 4, 32, "v7fbdb5f641f0"],
    "v7fbdb5f69a90": ["INPUT_REGISTER_EAX_32", 11, 32],
    "v7fbdb5f6af20": ["INPUT_REGISTER_EBX_32", 11, 32],
    "v7fbdb5f58170": ["INPUT_REGISTER_CF_1", 11, 1],
    "v7fbdb5f82cf0": ["INPUT_REGISTER_PF_1", 11, 1],
    "v7fbdb5f81470": ["INPUT_REGISTER_AF_1", 11, 1],
    "v7fbdb5f55dc0": ["INPUT_REGISTER_ZF_1", 11, 1],
    "v7fbdb5f582c0": ["INPUT_REGISTER_SF_1", 11, 1],
    "v7fbdb5f6ffa0": ["INPUT_REGISTER_OF_1", 11, 1],
    "v7fbdb5f817d0": ["OUTPUT_REGISTER_EAX_32", 12, 32],
    "v7fbdb5f85ba0": ["OUTPUT_REGISTER_EBX_32", 12, 32],
    "v7fbdb5f847f0": ["OUTPUT_REGISTER_CF_1", 12, 1],
    "v7fbdb5f66110": ["OUTPUT_REGISTER_PF_1", 12, 1],
    "v7fbdb5f815c0": ["OUTPUT_REGISTER_AF_1", 12, 1],
    "v7fbdb5f82bb0": ["OUTPUT_REGISTER_ZF_1", 12, 1],
    "v7fbdb5f72f10": ["OUTPUT_REGISTER_SF_1", 12, 1],
    "v7fbdb5f84960": ["OUTPUT_REGISTER_OF_1", 12, 1],
    "v7fbdb5f69960": ["INSTRUCTION_BITS_16", 13, 16],
    "v7fbdb5f646b0": [
        "OUTPUT_REGISTER_CHECK_EAX_32",
        14,
        1,
        "v7fbdb5f834a0",
        "v7fbdb5f817d0",
    ],
    "v7fbdb5f767f0": [
        "OUTPUT_REGISTER_CHECK_CF_1",
        14,
        1,
        "v7fbdb5f721f0",
        "v7fbdb5f847f0",
    ],
    "v7fbdb5f7a210": [
        "OUTPUT_REGISTER_CHECK_PF_1",
        14,
        1,
        "v7fbdb5f64880",
        "v7fbdb5f66110",
    ],
    "v7fbdb5f76f50": [
        "OUTPUT_REGISTER_CHECK_AF_1",
        14,
        1,
        "v7fbdb5f79250",
        "v7fbdb5f815c0",
    ],
    "v7fbdb5f59330": [
        "OUTPUT_REGISTER_CHECK_ZF_1",
        14,
        1,
        "v7fbdb5f65520",
        "v7fbdb5f82bb0",
    ],
    "v7fbdb5f6fe40": [
        "OUTPUT_REGISTER_CHECK_SF_1",
        14,
        1,
        "v7fbdb5f79540",
        "v7fbdb5f72f10",
    ],
    "v7fbdb5f7bb60": [
        "OUTPUT_REGISTER_CHECK_OF_1",
        14,
        1,
        "v7fbdb5f587b0",
        "v7fbdb5f84960",
    ],
    "v7fbdb5f7fd50": [
        "OUTPUT_REGISTER_CHECK_EBX_32",
        14,
        1,
        "v7fbdb5f834a0",
        "v7fbdb5f85ba0",
    ],
    "v7fbdb5f7b110": [
        "PRESERED_REGISTER_CHECK_EBX_32",
        15,
        1,
        "v7fbdb5f6af20",
        "v7fbdb5f85ba0",
    ],
    "v7fbdb5f7efa0": [
        "PRESERED_REGISTER_CHECK_EAX_32",
        15,
        1,
        "v7fbdb5f69a90",
        "v7fbdb5f817d0",
    ],
    "v7fbdb5f928f0": [
        "INSTRUCTION_BITS_CHECK_3",
        17,
        1,
        "v7fbdb5f63290",
        "v7fbdb5f75920",
    ],
    "v7fbdb5f6d640": [
        "INSTRUCTION_BITS_CHECK_2",
        17,
        1,
        "v7fbdb5f801f0",
        "v7fbdb5f95520",
    ],
    "v7fbdb5f7fa70": [
        "INSTRUCTION_BITS_CHECK_1",
        17,
        1,
        "v7fbdb5f88740",
        "v7fbdb5f6b780",
    ],
    "v7fbdb5f84540": [
        "INSTRUCTION_BITS_CHECK_2",
        17,
        1,
        "v7fbdb5f84670",
        "v7fbdb5f7a510",
    ],
    "v7fbdb5f684e0": [
        "INSTRUCTION_BITS_CHECK_8",
        17,
        1,
        "v7fbdb5f800b0",
        "v7fbdb5f61390",
    ],
    "v7fbdb5f74eb0": [
        "INSTRUCTION_BITS_CHECK_2",
        17,
        1,
        "v7fbdb5f84670",
        "v7fbdb5f95520",
    ],
    "v7fbdb5f58f20": [
        "INSTRUCTION_BITS_CHECK_2",
        17,
        1,
        "v7fbdb5f801f0",
        "v7fbdb5f7a510",
    ],
    "v7fbdb5f682a0": [
        "ALL_OF_13",
        18,
        1,
        "v7fbdb5f928f0",
        "v7fbdb5f6d640",
        "v7fbdb5f7fa70",
        "v7fbdb5f84540",
        "v7fbdb5f684e0",
        "v7fbdb5f646b0",
        "v7fbdb5f7b110",
        "v7fbdb5f767f0",
        "v7fbdb5f7a210",
        "v7fbdb5f76f50",
        "v7fbdb5f59330",
        "v7fbdb5f6fe40",
        "v7fbdb5f7bb60",
    ],
    "v7fbdb5f651f0": [
        "ALL_OF_13",
        18,
        1,
        "v7fbdb5f928f0",
        "v7fbdb5f74eb0",
        "v7fbdb5f7fa70",
        "v7fbdb5f58f20",
        "v7fbdb5f684e0",
        "v7fbdb5f7efa0",
        "v7fbdb5f7fd50",
        "v7fbdb5f767f0",
        "v7fbdb5f7a210",
        "v7fbdb5f76f50",
        "v7fbdb5f59330",
        "v7fbdb5f6fe40",
        "v7fbdb5f7bb60",
    ],
    "v7fbdb5f85590": ["ONE_OF_2", 21, 1, "v7fbdb5f682a0", "v7fbdb5f651f0"],
}

# this produces an ordering of peter's IR based on dependencies, so that we can have lines of code from it
def make_ordering(operations):
    ordering = []

    permanently_marked = set()
    temporarily_marked = set()
    nodes = list(operations.keys())

    # DFS to get an ordering on the graph
    def visit(node):
        if node in permanently_marked:
            return
        if node in temporarily_marked:
            raise Exception("operations is not a DAG!", node)
        temporarily_marked.add(node)
        for other_node in operations[node][1:]:
            if type(other_node) is str:
                visit(other_node)
        temporarily_marked.remove(node)
        permanently_marked.add(node)
        ordering.append(node)

    result_node = ""
    # 'RESULT' should be the first node, but just in case it's not check the whole thing
    # this will/should short circuit after the first iteration
    for name, ops in operations.items():
        if ops[0] == "RESULT":
            result_node = name
            break

    if result_node == "":
        raise Exception("no result node")

    # instead of visiting the entire DAG, only visit stuff that gets factored into the result
    visit(result_node)
    return ordering


# this function inlines constants, as well as removing inputs from variable assignments
# this means there will be clear names for initial registers and fewer variable assignments overall
def inline_inputs_constants(ordering, operations):
    def inliner(key, replacement):
        for node in ordering:
            ops = operations[node]
            for i, x in enumerate(ops):
                if x == key:
                    operations[node][i] = replacement

    inputs = []

    # inline inputs & constants
    for node in ordering.copy():
        ops = operations[node]
        if len(ops) == 3:
            if "CONST" in ops[0]:  # its a constant
                const = ops[0].split("_")
                const_value = f"'b {const[2]}"
                inliner(node, const_value)
                ordering.remove(node)
                del operations[node]
            else:  # its an input
                input_name = ops[0]
                input_length = ops[2]
                inliner(node, input_name)
                ordering.remove(node)
                del operations[node]
                inputs.append((input_name, input_length))

    return (inputs, ordering, operations)


llvm_infixes = {
    "LLVM_and_": "&",
    "LLVM_or_": "|",
    "LLVM_xor_": "^",
    "LLVM_lshr_": ">>",
    "LLVM_add_": "+",
    "LLVM_icmp_eq_": "==",
    "LLVM_icmp_ult_": "<",
    "LLVM_icmp_ne_": "!=",
}

def wire_declare_string(wire_name, wire_size):
    if wire_size == 1:
        return f"wire {wire_name}"
    return f"wire [0:{wire_size - 1}] {wire_name}"

def variable_assign_string(wire_name, assignment):
    return f"assign {wire_name} = {assignment}"

# this passes through the ordering of the DAG and deduces types and code for each variable assign as it goes along
def compute_verilog(inputs, ordering, operations):
    inputs_outputs = []
    for inp in inputs:
        inputs_outputs.append(f"input  {wire_declare_string(inp[0], inp[1])}")
    inputs_outputs.append("output wire RESULT")

    variable_assigns = []
    wire_declarations = []

    for node in ordering:
        ops = operations[node]
        isplit = ops[0].split("_")
        if isplit[0] == "RESULT":
            variable_assigns.append(("RESULT", ops[3]))
        else:
            wire_declarations.append(wire_declare_string(node, ops[2]))
            if isplit[0] == "EXTRACT":
                variable_assigns.append((node, f"{ops[5]}[{ops[3] - 1}:{ops[4]}];"))
            elif isplit[0] == "ALL" and isplit[1] == "OF":
                variable_assigns.append((node, " & ".join(ops[3:])))
            # @pag is this right?
            elif isplit[0] == "ONE" and isplit[1] == "OF":
                variable_assigns.append((node, " | ".join(ops[3:])))
            elif "LLVM" == isplit[0]:
                llvm_instr = "".join([i for i in ops[0] if not i.isdigit()])
                if llvm_instr in llvm_infixes:
                    variable_assigns.append(
                        (node, " ".join([ops[5], llvm_infixes[llvm_instr], ops[6]]))
                    )
                else:
                    variable_assigns.append(
                        (node, "{* PLACEHOLDER FOR " + str(ops) + " *}")
                    )
            else:
                variable_assigns.append(
                    (node, "{* PLACEHOLDER FOR " + str(ops) + " *}")
                )

    final_string = "module SieveCircuit(\n  "
    final_string += ",\n  ".join(inputs_outputs)
    final_string += ",\n);\n  "
    final_string += ";\n  ".join(wire_declarations)
    final_string += "\n  "
    final_string += ";\n  ".join(
        [
            variable_assign_string(node, assignment)
            for node, assignment in variable_assigns
        ]
    )
    final_string += "\nendmodule"
    return final_string


ordering = make_ordering(circuit)
# for node in ordering:
#    print(node, circuit[node])
inputs, ordering, circuit = inline_inputs_constants(ordering, circuit)
print(compute_verilog(inputs, ordering, circuit))
