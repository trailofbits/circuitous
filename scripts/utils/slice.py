# Copyright (c) 2021 Trail of Bits, Inc.

import json
import argparse

# Lifting greater amounts of instructions will produce some really hard to
# navigate circuits. This utility script tries to provide some basic
# functionality to deal with it - for now slicing, maybe
# some forms of coloring later.

class JsonNode:
  __slots__ = ("id", "op_name", "op_code", "node_type", "uses",
               "raw_operands", "users")

  def __init__(self, id_, data_):
    self.id = id_
    self.op_name = data_["op_name"]
    self.op_code = data_["op_code"]
    self.node_type = data_["node_type"]
    self.uses = []
    self.users = []
    self.raw_operands = data_["operands"]

  def as_json(self):
    out = {}
    out["op_name"] = self.op_name
    #out["op_code"] = self.op_code
    #out["node_type"] = self.node_type
    out["operands"] = self.raw_operands
    return self.id, out

  def use(self, other):
    if other not in self.uses:
      self.uses.append(other)
    if self not in other.users:
      other.users.append(self)
    if other.id not in self.raw_operands:
      self.raw_operands.append(other.id)

  def get_as_orphan(self):
    return JsonNode(self.id, self.data())

  def data(self):
    return { "op_name" : self.op_name,
             "op_code" : self.op_code,
             "node_type" : self.node_type,
             "operands" : []}

class JsonCircuit:
  __slots__ = ("nodes", "id")

  def __init__(self, id_):
    self.nodes = {} # id -> JsonNode
    self.id = id_

  def add(self, id, data):
    if id not in self.nodes:
      self.nodes[id] = JsonNode(id, data)
    return self.nodes[id]

  def add_node(self, node):
    if node.id not in self.nodes:
      self.nodes[node.id] = node
    return self.nodes[node.id]

  def finalize(self):
    for id, node in self.nodes.items():
      for r_op in node.raw_operands:
        assert r_op in self.nodes
        node.use(self.nodes[r_op])

  def as_json(self):
    out = [self.id]
    out_nodes = {}
    for _, node in self.nodes.items():
      n_id, n_data = node.as_json()
      out_nodes[n_id] = n_data
    out.append(out_nodes)
    return out

  def get(self, what):
    out = []
    for _, node in self.nodes.items():
      if what == node.op_name:
        out.append(node)
    #assert len(out) == 1, "TODO: Allow slicing from multiple roots"
    if len(out) != 1:
      print("Slicing from multiple roots")
    return out


class JsonSlicer:
  __slots__ = ("circuit", "sliced", "up_stops", "seen_u", "seen_d")

  def add_sliced(self, root):
    node = root.get_as_orphan()
    return self.sliced.add_node(node)

  def __init__(self, circuit_):
    self.circuit = circuit_
    self.sliced = JsonCircuit(self.circuit.id)
    self.up_stops = {"ALL_OF"}
    self.seen_u = set()
    self.seen_d = set()


  def slice(self, what):
    for root in self.circuit.get(what):
      mirror = self.add_sliced(root)
      self.down(root, mirror)
      self.up(root, mirror)
    return self

  def up_stop(self, what):
    for prefix in self.up_stops:
      if prefix in what.op_name:
        return True
    return False

  def up(self, root, mirror):
    if root.id in self.seen_u:
      return
    else:
      self.seen_u.add(root.id)
    for user in root.users:
      if not self.up_stop(user):
        up_mirror = user.get_as_orphan()
        up_mirror = self.sliced.add_node(up_mirror)
        up_mirror.use(mirror)
        self.up(user, up_mirror)
      else:
        up_mirror = user.get_as_orphan()
        up_mirror = self.sliced.add_node(up_mirror)
        up_mirror.use(mirror)
    if "OUTPUT_REGISTER_CHECK" in root.op_name:
      self.up(root, mirror)
      self.down(root, mirror)

  def down(self, root, mirror):
    if root.id in self.seen_d:
      return
    else:
      self.seen_d.add(root.id)
    for use in root.uses:
      down_mirror = self.add_sliced(use)
      mirror.use(down_mirror)
      self.down(use, down_mirror)
    if "HINT_" in root.op_name:
      for user in root.users:
        if "HINT_CHECK" in user.op_name:
          mirror_up = self.add_sliced(user)
          mirror_up.use(mirror)
          self.down(user, mirror_up)
          self.up(user, mirror_up)

  def get(self):
    return self.sliced

class IDS:
  __slots__ = ("map", "counter")

  def __init__(self):
    self.map = {}
    self.counter = 0

  def translate(self, hash):
    if hash not in self.map:
      self.map[hash] = self.counter
      self.counter += 1
    return self.map[hash]


def load(path):
  with open(path) as f:
    result = json.load(f)
    circuit = JsonCircuit(result[0])
    for id, data in result[1].items():
      circuit.add(id, data)
    circuit.finalize()
    return circuit


def store_json(path, circuit):
  with open(path, 'w') as f:
    x = circuit.as_json()
    nodes = x[1]
    translated = {}
    table = IDS()
    for id, node in nodes.items():
      new_ops = []
      for op in node["operands"]:
        new_ops.append(table.translate(op))
      node["operands"] = new_ops
      translated[table.translate(id)] = node
    x[1] = translated
    json.dump(x, f, indent=2)

def store_dot(path, circuit):
  with open(path, 'w') as f:
    x = circuit.as_json()
    nodes = x[1]
    translated = {}
    table = IDS()
    for id, node in nodes.items():
      new_ops = []
      for op in node["operands"]:
        new_ops.append(table.translate(op))
      node["operands"] = new_ops
      translated[table.translate(id)] = node
    x[1] = translated
    f.write("strict digraph {\n")
    for id, node in translated.items():
      formatted = f'{id} [label = {node["op_name"]}]\n'
      f.write(formatted)
    for id, node in translated.items():
      for trg in node["operands"]:
        formatted = f'{id} -> {trg}\n'
        f.write(formatted)
    f.write("}\n")




def main():
  arg_parser = argparse.ArgumentParser(
    formatter_class = argparse.RawDescriptionHelpFormatter)
  arg_parser.add_argument("--slice_oreg",
                          help="Slice the file by output reg")
  arg_parser.add_argument("--slice_ireg",
                          help="Slice the file by input reg")
  arg_parser.add_argument("--json_in", help="Input json file")
  arg_parser.add_argument("--json_out", help="Output json file")
  arg_parser.add_argument("--dot_in", help="Input dot file")
  arg_parser.add_argument("--dot_out", help="Output dot file")

  args, command_args = arg_parser.parse_known_args()

  print(args.json_in, args.json_out)
  assert (args.json_in is not None) == (args.json_out is not None)
  #assert (args.dot_in is not None) == (args.dot_out is not None)

  circuit = load(args.json_in)
  sliced = JsonSlicer(circuit).slice("OUTPUT_REGISTER_CHECK_AF_1").get()
  store_json(args.json_out, sliced)
  if args.dot_out is not None:
    store_dot(args.dot_out, sliced)

if __name__ == "__main__":
  main()