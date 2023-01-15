# High level overview

Our goal for the semantics emitter generator is to produce a c++ source file, which contains the code to call an end-user provided visitor which describes the semantics for a given byte sequence.

To understand how we generate the semantics emitter I'll first describe what will be generated how pieces interlink.

### The generated artifact
The generated code consists of 3 large parts:
1. The instruction identifier (accepts/rejects an byte sequence )
2. The "decoder" (Sets up the actual instruction semantics)
3. The semantics emitter (A fancy optimized visitor dispatcher)


#### The instruction identifier
This provides the function that accepts an byte sequence/instruction bytes and does one of two options.
1. It rejects the instruction as a valid instruction
2. It calls the correct decoder function and passes along the input byte

#### The decoder 
The decoder part consists of 1 function for every context.
Every function accepts a byte sequence and specializes it to a concrete instruction.
Example: We might have a single context for every version of `mov reg, reg`. A single decoder function would be produced and it would
look at the specific instruction bits to identify if it's a `mov rax, rcx` or `mov r15, rsp`.

The actual semantics describing an instruction are grouped here, before they are given to the semantics emitter which calls
the user-provided visitor. The creation of the data structure passed to the semantics emitter has 2 parts.
The first part are all those semantics which belong to every instruction inside the context.
The second part is the instruction specific part of the context. 
i.e the part that would specify the registers for `mov reg, reg`.
These instruction specific parts are guarded by "decode-time selects". Aka the set of all choices for select nodes which 
transitively depend solely on the instruction bits. 

For every assignment done in the context, we create call the semantics emitter exactly once. 

#### Semantics emitter 
The semantics emitter is a fancy and globally optimized visitor call dispatcher. It's shape is determined by the Semantics Emitter Generator Graph (SEG graph) 
This graph has a couple of properties:
- It contains copy of all sub-trees in the circIR graph that represent an assignment
- All nodes have any information stripped like their size and operation kind
- All sub-graphs which are isomorphic have been merged, meaning that there should be no two different sub-trees which share the same shape.
- As a consequence this means that the graph is now a multi-graph.
- During merging we remember/merge which assignment in for what _instruction_ (not context) this sub-tree came from. 

We transform this graph into a code. The actual output depends on a hyper-parameter which considers the design cost as a function.
This hyper-parameter determines how often a sub-tree needs to be called to have its own helper function in the generated code.
But more on this later.

 
# Pipeline
The pipeline from circuit to generation is:
1. Apply a graph analysis tainting all nodes whether they are only dependent on decode time evaluateble nodes
2. For every context/VI we extract all assignments done
3. For every assignment, we create an isomorphic graph (disregarding operation kind) for _every_ way an assignment can be specialized during decode time. 
4. We feed all of these options into the node sharing optimizer, giving back a Semantics Emitter Generator Graph (SEG Graph)
5. Compute costs over the node sharing optimizer graph 
6. Emit semantics emitter functions
7. For every context, specialize and call the semantics emitter providing all arguments for the end-user visitor.

